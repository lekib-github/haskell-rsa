# RSA Encryption Algorithm in Haskell

## RSA Overview

RSA is one of the oldest and most widely used public-key cryptosystems. Public-key refers to the fact that there are two distinct keys for encryption and decryption, the public and private key respectively. The security of the algorithm comes down to the difficulty of factoring the product of two large primes with traditional computing methods.

## Implementation Plan

The algorithm is made up of four steps: [Key Generation](https://en.wikipedia.org/wiki/RSA_(cryptosystem)#Key_generation), [Key Distribution](https://en.wikipedia.org/wiki/RSA_(cryptosystem)#Key_distribution), [Encryption](https://en.wikipedia.org/wiki/RSA_(cryptosystem)#Encryption), and [Decryption](https://en.wikipedia.org/wiki/RSA_(cryptosystem)#Decryption).

This project will focus on implementing a key generation function (random ints, Miller-Rabin, Carmichael totient for primes, Euclid's algorithm, extended Euclid's algorithm), and being able to encrypt and decrypt messages (numeric representation <-> string, fast powMod function, block-by-block encryption/decryption) via IO. We assume the public key is available, and no key distribution over the internet or otherwise will be handled by the program. The tool can be used in the command line via a script called "rsa". Public key can be used for *encryption*, private key for *signing*.

## Usage
rsa -keygen [n] &emsp; Generate keys of bit-length **n** to files pub.key and priv.key respectively.

rsa -encrypt [KEY] [FILE] &emsp; Encrypt the given **file** contents with the **key** to std out.

rsa -decrypt [KEY] [FILE] &emsp; Decrypt the given **file** contents with the **key** to std out.
