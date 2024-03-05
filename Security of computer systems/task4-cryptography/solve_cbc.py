from curses.ascii import isspace
from pwn import remote, hashlib
from sys import argv
import sys
import time

DEBUG = False

def bytes_to_hex(data):
    return (data.hex()).encode()


def from_hex_to_bytes(data):
    return bytes.fromhex(data.decode())


def xor(a, b):
    return bytes([ac ^ bc for ac, bc in zip(a, b)])


def pad(msg):
    byte = 16 - len(msg) % 16
    return msg + bytes([byte] * byte)


def unpad(msg):
    if not msg:
        return b''
    return msg[:-msg[-1]]


def progressbar(it, prefix="", size=60, out=sys.stdout): # Python3.6+
    """
        Displays progressbar, taken from https://stackoverflow.com/a/34482761.
    """
    count = len(it)
    start = time.time()
    def show(j):
        x = int(size*j/count)
        remaining = ((time.time() - start) / j) * (count - j)
        
        mins, sec = divmod(remaining, 60)
        time_str = f"{int(mins):02}:{sec:05.2f}"
        
        print(f"{prefix}[{u'█'*x}{('.'*(size-x))}] {j}/{count} Est wait {time_str}", end='\r', file=out, flush=True)
        
    for i, item in enumerate(it):
        yield item
        show(i+1)
    print("\n", flush=True, file=out)


def byte_generator():
    for i in range(256):
        yield bytes([i])


def smart_byte_generator():
    """
        Generates bytes, but readable characters are earlier in sequence.
    """
    # generate small letters first with {, |, |, ~
    for i in range(97, 123):
        yield bytes([i])
    # then numbers
    for i in range(48, 58):
        yield bytes([i])
    # then big letters
    for i in range(65, 91):
        yield bytes([i])
    # then special characters
    for i in range(32, 48):
        yield bytes([i])
    
    # then the rest
    for i in range(58, 65):
        yield bytes([i])
    for i in range(91, 97):
        yield bytes([i])
    for i in range(123, 256):
        yield bytes([i])
    for i in range(0, 32):
        yield bytes([i])


def next_prefixes_generator(current_prefix=b""):
    for n in range(256):
        yield current_prefix + bytes([n])


def get_key_from_value(d, value):
    return list(d.keys())[list(d.values()).index(value)]


class DecryptionFailed(Exception):
    pass


class MessageDecipher:
    """
        Helper class to decipher the message.
    """
    HELLO_MSG = b"Hello"
    IV = b'so_random_iv_wow'
    
    def __init__(self, r, flag_request):
        self.r = r
        self.flag_request = flag_request
        handshake = r.recvline().strip()
        handshake = from_hex_to_bytes(handshake)
        self.hello_encrypted = handshake[16:]
        print("Received handshake:", handshake)
        
        print(f"Sending '{flag_request} message")
        self.flag_response = self.send_message(flag_request)
        self.encrypted_flag = from_hex_to_bytes(self.flag_response)

        print(f"Sending unkown command message")
        self.unknown_comm_response = self.send_message(b"funny task")
        
        self.last_byte_to_encrypted = self.generate_last_byte_to_encrypted_dict()
        self.prefix_to_response = dict()



    def generate_last_byte_to_encrypted_dict(self):
        """
            Generates dictionary with usefull hashes,
            that contains last specific last byte.
            When unpadding the message on server it will
            cut that number of bytes from the end.
            Hash has always length of 32 bytes and we would like
            to keep only 1/2/3/4 bytes of the previous block, 
            for example 32 + 15 = 47 etc.
        """
        last_byte_to_encrypted = dict()
        
        last_byte_to_pref = dict()
        last_byte_to_pref[44] = b'\x00\xf5'
        last_byte_to_pref[45] = b'\xa9'
        last_byte_to_pref[46] = b'\x9d'
        last_byte_to_pref[47] = b'\xd8'

        for key, msg in last_byte_to_pref.items():
            response = self.send_message(b"hash?" + msg)
            last_byte_to_encrypted[key] = from_hex_to_bytes(response)[16:48]

        return last_byte_to_encrypted
    

    def get_encrypted_msg(self, message):
        """
            Returns 32 bytes of encrypted message,
            that decrypts to specific text given in the argument.
            First 16 bytes has to be Inicialization Vector.
        """
        previous_aes_input = xor(pad(b"Hello"), self.IV)
        modified_iv = xor(previous_aes_input, message)
        return modified_iv + self.hello_encrypted
    

    def get_encrypted_msg_from_hash(self, prefix_to_hash, message):
        """
            Returns 32 bytes of encrypted message,
            that decrypt to specific text given in the argument.
            Differs from get_encrypted_msg in encrypted message.
        """
        hash = hashlib.sha256(prefix_to_hash).digest()
        response = from_hex_to_bytes(self.prefix_to_response[prefix_to_hash])
        previous_aes_input = xor(hash[:16], response[:16])
        modified_iv = xor(previous_aes_input, message)
        return modified_iv + response[16:32]


    def send_message(self, message):
        """
            Sends encrypted message that decrypts to specific
            text specified in an argument.
            Modifies known "Hello" message.
        """
        previous_aes_input = xor(pad(b"Hello"), self.IV)
        modified_iv = xor(previous_aes_input, pad(message))
        self.r.sendline(bytes_to_hex(modified_iv + self.hello_encrypted))

        return  self.r.recvline().strip()
    

    def get_prefix_from_response(self, response, current_prefix, magic_xor=0):
        """
            Given hash response, where hash is created using unkown prefix (1 byte 
            of prefix is unkonwn), find the prefix.
            Search and save all possibilities in dictionary.

            Argument "xored_value" is helper value, that xored with byte
            will decode to specific byte on the server. That gives us the ability
            to check some bytes in specific order, for example trying small letters first. 
        """
        if response in self.prefix_to_response.values():
            print("Got cached prefix.")
            return get_key_from_value(self.prefix_to_response, response)
        else:
            expanded_prefixes = [current_prefix + xor(b, bytes([magic_xor])) for b in smart_byte_generator()]
            for new_prefix in progressbar(expanded_prefixes, "Searching for proper hash: ", 30):
                if new_prefix not in self.prefix_to_response.keys():
                    new_response = self.send_message(b"hash?" + new_prefix)
                    self.prefix_to_response[new_prefix] = new_response
                
                if self.prefix_to_response[new_prefix] == response:
                    print(f"Got {len(self.prefix_to_response)} hashes fetched from server.")
                    return new_prefix
                
            print("No prefixes found because of whitespace characters...")
            return None
        

    def get_hash_prefix_response(self, xor_magic, current_prefix, iv, hash_str_block, block, unpad_hash):
        self.r.sendline(bytes_to_hex(iv + hash_str_block + block + unpad_hash))
        response = self.r.recvline().strip()
        new_prefix_xored = self.get_prefix_from_response(response, xor(current_prefix, hash_str_block),
                                                         xor_magic)
        if new_prefix_xored is None:
            return None

        new_prefix = xor(hash_str_block, new_prefix_xored)
        return new_prefix


    def decrypt_first_4_bytes_of_block(self, prev_block, block):
        """
            Decrypts first 4 bytes of block using unpad
            mistake and saved encrypted hashes.
        """
        print("\nFinding first 4 bytes with hashes...")
        hash_str = b" " * 11 + b"hash?"
        hash_str_encrypted = self.get_encrypted_msg(hash_str)
        hash_str_block = hash_str_encrypted[16:]
        hash_str_iv = hash_str_encrypted[:16]

        current_prefix = b"" # prefix of aes_input
        for i in range(1, 5):
            new_prefix = self.get_hash_prefix_response(prev_block[i - 1] ^ hash_str_block[i - 1],
                                                       current_prefix, hash_str_iv, hash_str_block,
                                                       block, self.last_byte_to_encrypted[48 - i])


            print("New block prefix: ", xor(new_prefix, prev_block))
            if new_prefix is None:
                raise DecryptionFailed("Got multiple spaces or newlines in xored hash, can't continue.")
            
            if new_prefix == current_prefix:
                print("Got whitespace in hash...")
                for prefix in self.prefix_to_response.keys():
                    """ We change the encrypted hash string, so maybe when block argument
                        xored with something else won't give whitespace character."""
                    new_hash_str_encrypted = self.get_encrypted_msg_from_hash(prefix, hash_str)
                    new_hash_iv = new_hash_str_encrypted[:16]
                    new_hash_block = new_hash_str_encrypted[16:]
                    new_prefix = self.get_hash_prefix_response(prev_block[i - 1] ^ new_hash_block[i - 1],
                                                            current_prefix, new_hash_iv, new_hash_block,
                                                            block, self.last_byte_to_encrypted[48 - i])
                    if new_prefix != current_prefix:
                        break
                else:
                    raise DecryptionFailed("Got multiple spaces or newlines in xored hash, can't continue.")
            current_prefix = new_prefix

        return xor(prev_block, current_prefix)
    

    def find_next_byte(self, old_iv, encrypted_blocks, known_prefix):
        """
            Given some prefix bigger than 3, we can search for the next one
            with 256 possibilities, swapping first bytes to spaces and "flag" keyword.
            When next byte is equal "?" server would respond with flag, otherwise with
            "unkown command" response.
        """
        
        for b in progressbar(list(smart_byte_generator()), "Checking for next byte: ", 30):
            suspected_flag_prefix = known_prefix + b
            target_plaintext = (len(suspected_flag_prefix) - 5) * b" " + self.flag_request

            """ 
            Let's assumet that: suspected_flag_prefix == known_plaintext
                known_plaintext = aes_input ^ iv
                suspected_flag_prefix ^ target_plaintext = aes_input ^ iv ^ target_plaintext
                target_plaintext = aes_input ^  iv ^ target_plaintext ^ suspected_flag_prefix
                target_plaintext = aes_input ^ (              new_iv                  ) 
            """
            new_iv = xor(old_iv, xor(suspected_flag_prefix, target_plaintext)) \
                    + old_iv[len(suspected_flag_prefix):]
            
            self.r.sendline(bytes_to_hex(new_iv + encrypted_blocks))
            response = self.r.recvline().strip()
            
            if response != self.unknown_comm_response:
                return b

        print("Failed to find next byte")
        return b""


    def decrypt_first_block(self, prev_block, encrypted_blocks, prefix=b""):
        """
            Given previous block and all the block to the right decrypt
            the block.
        """
        if len(prefix) < 4:
            prefix = self.decrypt_first_4_bytes_of_block(prev_block, encrypted_blocks[:16])

        for i in range(len(prefix), 16):
            new_byte = self.find_next_byte(prev_block, encrypted_blocks, prefix)
            if new_byte == b"":
                return prefix
            prefix += new_byte
            print("Finding next byte... Current prefix:", prefix)
        
        return prefix
    

    def decrypt_message(self, msg, known_prefix=b""):
        """
            Decrypt all message. If known_prefix is specified, it will be reduce
            the time of script.
        """
        plaintext = b""
        plaintext += self.decrypt_first_block(msg[:16], msg[16:], known_prefix)

        if self.flag_request == b"flag?":
            return plaintext

        for i in range(16, len(msg) - 16, 16):
            plaintext += self.decrypt_first_block(msg[i:i+16], msg[i+16:])
            print("Got plaintext: ", plaintext)

        return plaintext



def run_task(address, port, flag_message):
    r = remote(address, port)

    r.recvuntil(b"2) Block cipher (two flags here)\n")
    r.sendline(b"2")
    r.recvuntil(b"> ")

    com = MessageDecipher(r, flag_message)
    result = com.decrypt_message(com.encrypted_flag)
    r.close()

    return result


if __name__ == '__main__':
    if len(argv) != 3:
        print(f"Usage: {argv[0]} <address> <port>")
        exit(1)

    address = argv[1]
    port = int(argv[2])
    result = None
    while result is None:
        try:
            result = run_task(address, port, b"flag?")
        except DecryptionFailed as error:
            pass
    flag1 = result
    print(flag1, "\n")

    result = None
    while result is None:
        try:
            result = run_task(address, port, b"FLAG!")
        except DecryptionFailed as error:
            pass
    flag2 = result
    print(flag2, "\n")
    
    print("\n================= FLAG 1 ==================")
    print(flag1)

    print("\n================= FLAG 2 ==================")
    print(flag2)

    print()





    

    

