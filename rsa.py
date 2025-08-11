import random
import os
from sympy import isprime

# To generate random large prime numbers for keys
def randPrime():
    while True:
    	num = random.randint(100, 10000)
    	if isprime(num):
    		return num
    		
# To search for decryption    		
def findFile(name, path):
	for root, dirs, files in os.walk(path):
		if name in files:
			return os.path.join(root, name)
	return None
		

# To scrable/reconstitue the message: result = (base^exp) % m
def power(base, exp, m):
    result = 1
    base = base % m
    while exp > 0:
        if exp & 1:
            result = (result * base) % m
        base = (base * base) % m
        exp = exp // 2
    return result

# To solve "d" in the equation: (e*d)%t=1
def modInverse(e, t):
    for d in range(2, t):
        if (e * d) % t == 1:
            return d
    return -1
    
# To find "e" such that "e" and "t" can only be commonly divided by one
def coPrime(t):
    for e in range(2, t):
        if gcd(e, t) == 1:
            return e
    return -1

# RSA Key Generation
def generateKeys():
    p = randPrime()
    q = randPrime()
    
    n = p * q
    #t is short for temp
    t = (p - 1) * (q - 1)

    # To find the earliest co-prime "e" with "t"
    e = coPrime(t)

    # To find the earliest modular inverse for variable "d"
    d = modInverse(e, t)

    return n, e, d

# Function to calculate gcd
def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a

# Encrypt message using public key (n, e)
def encrypt(m, n, e):
    return power(m, n, e)

# Decrypt message using private key (n, d)
def decrypt(c, n, d):
    return power(c, n, d)

# Main
if __name__ == "__main__":
    
	check = ""

	# TO ask wether the user wants to encrypt or decrypt
	while check != "e" and check != "d":
		check = input("Encrypt number message into file (e) or decrypt number message in file (d): ")
		if check != "e" and check != "d":
			print(f"Invalid Input Try Again!")
    
    
	if check == "e":
	    # Key Generation
	    n, e, d = generateKeys()
	    
	    # Original Message / Plaintext
	    original = int(input("Enter number message: "))
	    print(f"Original Message: {original}")

	    # Listing public and Private keys
	    print(f"Public Key (e, n): ({e}, {n})")
	    print(f"Private Key (d, n): ({d}, {n})")

	    # Encrypt aMessage / Convert to Ciphertext and outpu to file
	    encrypted = encrypt(original, e, n)
	    with open("rsafile.txt", "w") as f:
	    	f.write(str(encrypted))
	    print(f"Encrypted Message: {encrypted}")
	    
	if check == "d":
	    
	    found = 0
	    tempfile = ""
	    enfile = None
	    
	    # To check if the readable exists
	    while found != 1:
	    	tempfile = input("Insert text file name for decryption: ")
	    	enfile = findFile(tempfile, '/home')
	    	if enfile != None:
	    		found = 1
	    	else:
	    		print(f"File Doesn't Exist Try Again!")
	    # To ask for the values for the private key		
	    d = int(input("Enter number value for 'd' in private key: "))
	    n = int(input("Enter number value for 'n' in private key: "))

	    # To read the number string from the inputted file
	    readenfile = open(enfile, "r")
	    encrypted = int(readenfile.read())
	    print(f"Encrypted Message: {encrypted}")

	    # Decrypt Message / Back to Plaintext
	    decrypted = decrypt(encrypted, d, n)
	    print(f"Decrypted Message: {decrypted}")
