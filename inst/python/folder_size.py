import os
import sys
import csv
from concurrent.futures import ThreadPoolExecutor, as_completed

def get_size(path):
    """Berechnet rekursiv die Größe eines Ordners oder einer Datei."""
    if os.path.isfile(path):
        return os.path.getsize(path)
    total = 0
    for root, _, files in os.walk(path):
        for f in files:
            fp = os.path.join(root, f)
            try:
                total += os.path.getsize(fp)
            except:
                pass
    return total

def folder_info_top_level(path, max_workers=None):
    """Gibt alle Top-Level-Einträge zurück, Unterordnergrößen werden rekursiv berechnet."""
    items = []
    entries = list(os.scandir(path))

    # Dateien im Top-Level
    for e in entries:
        if e.is_file():
            try:
                size = e.stat().st_size
            except:
                size = 0
            items.append([e.name, e.path, "file", size])

    # Unterordner im Top-Level, Größe parallel berechnen
    dirs = [e for e in entries if e.is_dir()]
    if dirs:
        if max_workers is None:
            max_workers = min(32, os.cpu_count() * 2)
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {executor.submit(get_size, d.path): d for d in dirs}
            for fut in as_completed(futures):
                d = futures[fut]
                size = fut.result()
                items.append([d.name, d.path, "directory", size])

    return items

if __name__ == "__main__":
    folder = sys.argv[1]
    writer = csv.writer(sys.stdout)
    writer.writerow(["name","path","type","size"])
    for row in folder_info_top_level(folder):
        writer.writerow(row)
