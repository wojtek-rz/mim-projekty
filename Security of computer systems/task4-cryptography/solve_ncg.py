from sys import argv
from pwn import *


table = []
numbers = [
    3864081771634699338,
    4836844088186330658,
    1996127217181336402,
    1301679432864466034,
    6897051607347875122
]

def generate_parameters(scope_bits, known_length, known_params: list):
    table = []
    max_value = 2 ** scope_bits
    step_value = 2 ** known_length
    for known_a, known_c in known_params:
        for a in range(known_a, max_value, step_value):
            for c in range(known_c, max_value, step_value):
                table.append((a, c))
    return table


def find_matching_params(number1, number2, paramas: list, scope_bits):
    max_value = 2 ** scope_bits
    number1 = number1
    number2 = number2 % max_value

    matching = []
    for a, c in paramas:
        res = ((number1 * a) ^ c) % max_value
        if res == number2:
            matching.append((a, c))

    return matching


def find_macthing_params_all(pair_numbers, params, scope_bits):
    print(f"Searching for a and c with scope {scope_bits} bits")

    first, second = pair_numbers[0]
    intersection = find_matching_params(first, second, params, scope_bits)

    for first, second in pair_numbers[1:]:
        res = find_matching_params(first, second, params, scope_bits)
        intersection = list(set(intersection) & set(res))
    
    print(f"Found {len(intersection)} matching a and c:")
    print(intersection)
    
    return intersection


def extract_last_bits(params, bits):
    mod = 2 ** bits
    return list(set([(a % mod, c % mod) for a, c in params]))



def find_params(numbers):
    pair_numbers = [(numbers[i], numbers[i+1]) for i in range(4)]
    known_length = 0
    known_params = [(0, 0)]
    scope = 4
    for i in range(64 - scope + 1):
        params = generate_parameters(scope + i, i, known_params)

        res = find_macthing_params_all(pair_numbers, params, scope + i)
        known_params = extract_last_bits(res, i + 1)

        print("Considering only last bits of a and c:")
        for elem in known_params:
            print(format(elem[0], "b"), format(elem[1], "b"))

        print()


    for i in range(64 - scope + 1, 64):
        params = generate_parameters(64, i, known_params)

        res = find_macthing_params_all(pair_numbers, params, 64)
        known_params = extract_last_bits(res, i + 1)

        print("Considering only last bits of a and c:")
        for elem in known_params:
            print(format(elem[0], "b"), format(elem[1], "b"))

        print()

    return res


def run_task(address, port):
    r = remote(address, port)

    r.recvuntil(b"2) Block cipher (two flags here)\n")
    r.sendline(b"1")
    r.recvuntil(b"> ")
    numbers = [int(r.recvline()) for i in range(5)]

    res_params = find_params(numbers)

    number = numbers[-1]
    for a, c in res_params:
        next_s = ((number * a) ^ c) % (2 ** 64)
        print(f"Found a and c: {str(a).rjust(25)}, {str(c).rjust(25)}    ---> \t{next_s}")

    r.sendline(str(next_s).encode())
    r.recvuntil(b"next?\n")
    flag = r.recvline()

    r.close()
    return flag


if __name__ == '__main__':
    if len(argv) != 3:
        print(f"Usage: {argv[0]} <address> <port>")
        exit(1)

    address = argv[1]
    port = int(argv[2])

    flag = run_task(address, port)
    print("\n\n================= FLAG ================")
    print(flag)
    print()