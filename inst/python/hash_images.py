# hash_images.py
import imagehash
from PIL import Image
import hashlib
import os
import csv

def sha256_hash(filepath):
    with open(filepath, 'rb') as f:
        return hashlib.sha256(f.read()).hexdigest()

def compute_hashes(image_path):
    try:
        img = Image.open(image_path)
        return {
            "path": image_path,
            "sha256": sha256_hash(image_path),
            "ahash": str(imagehash.average_hash(img)),
            "dhash": str(imagehash.dhash(img)),
            "phash": str(imagehash.phash(img)),
        }
    except:
        return None

def process_directory(img_dir, output_file):
    with open(output_file, "w", newline="") as csvfile:
        fieldnames = ["path", "sha256", "ahash", "dhash", "phash"]
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for root, dirs, files in os.walk(img_dir):
            for f in files:
                if f.lower().endswith((".jpg", ".jpeg", ".png")):
                    result = compute_hashes(os.path.join(root, f))
                    if result:
                        writer.writerow(result)

if __name__ == "__main__":
    import sys
    process_directory(sys.argv[1], sys.argv[2])
