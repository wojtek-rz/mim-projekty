# Assignment - Web Applications 
### Wojciech Rzepliński (438709)

The source code of the page written in the Django framework was investigated, and 3 serious vulnerabilities were found: Cross-Site Scripting (XSS), Path Traversal, and Server-Side Request Forgery. The service allows the creation of a greeting card and sending it to a selected person. We write the card in the WYSIWYG text editor, then it is sent in text form, but we can also download it as a PNG image.

All scripts used for attacks are located in separate files with appropriate names given in the descriptions.

## Session Theft via XSS - Flag 1

The lack of proper filtering of posts or CSP headers allows for sending any person a laurel with embedded `<script></script>`, which execute any script on the victim's browser. A dangerous vulnerability then is, for example, taking over the user's session token stored in the site's cookies, but here it has been rightly saved as HttpOnly, which prevents its reading by the javascript script. Similarly, sending forms is protected by the CSRF token, which prevents scripts from being sent between pages, but it does not protect against XSS scripts.

A potential attack looks like this:

- A site user, in their session, wants to send a card, but cannot do this through the built-in WYSIWYG editor because it will neutralize HTML tags. You need to do this using a raw POST request in your favorite coding tool.

- To avoid writing the query from scratch, we can write a demonstration laurel, then capture the request in the "network" section of developer tools and make a "copy as fetch". Then I put this script in my console in the browser on the attacked site.

- We replace the "content" field value in the fetch query with the script we want to run in the victim's browser. This script should:

    - Download the frontend form that we want to send from the victim's level.
    - Parse its HTML to extract the CSRF token from it.
    - Send a new POST request containing the contents of the form, placing the CSRF token in it. The sessionId cookie (not available to js scripts) will still be attached by the browser to each query, including the one we just created.

    In this way, we can for example send a laurel to anyone, as it is done via a CSRF-protected form. The `flag1.js` file contains a js script performing exactly such an attack, after pasting it into our site console (the laurel with the script will be sent from our account). The `xss_script` string contains the malicious script pasted into the laurel.

The first flag was located in the admin footer, so it was enough to send him a malicious script that will send us a laurel.

__FLAG: ThisIsTheFlagFromTheFooter__

## Path Traversal - Flag 2

The `utils.py` file includes the `render_card` function, which fills the card template with data by manually reading from the template file. The path to the template is defined by the "card.template" variable, which unfortunately does not have server-side data validation. The "select" field serves to enable only selected options in the form on the client side, but in HTML or in the browser's POST method we can still replace the "template" field value.

A potential attack looks like this: 
- Copying an analog request creating a card in the browser using "copy as fetch", then replacing the "template" field value with a malicious path and sending it from your browser again. The mechanism is the same as in the previous point.

- We insert our own path instead of the template, retreating arbitrarily using "../". To get the flag, we can notice that the `start.sh` script saves the flag in the `flag.txt` file at the server start. The program with the appropriate query is in the `flag2.sh` file.

- Then we will get a link to open the card in the answer, which will activate the `render_card` function when we enter it. It will read any file on the Docker machine and send us its contents.

__FLAG: AnotherFlagForPoints__

## Server-Side Request Forgery - Flag 3

Notice that generating an image is done by running the Chromium program, which will render the given page and save its image for us. This takes place on the server machine, which may have access to the internal network and in combination with the XSS vulnerability, Chromium will execute the script we have set.

A potential attack can look like this:

- The user creates a card in which he includes a malicious script (details as in the first vulnerability). This script can, for example, load a page from the server's internal network, e.g. some microservice, to which there is no external access. Of course, opening such a laurel on our laptop, we will be redirected to an address that the system will not recognize.

- Then we go to the "download-png" page with the card token we just created. The chromium process, after reading the page and the script, will load the page from the internal microservice, take a picture and send it back to us.

To get the flag, you need to connect to the secret microservice. The server and microservice are connected by a network in Docker, they can connect with each other using their name in the \path{docker-compose.yml} file. That's why the address will be: http://zad41-mimuw-finals-2023-super-secret-microservice:80, and the script will contain:
```
\begin{lst}
    <script>window.location = '...address...'</script>
\end{lst}
```

The scripts is located in `zad3.js`.

__FLAGA: 71a4b4fd2214b808e4942dfb06c717878399a04c__