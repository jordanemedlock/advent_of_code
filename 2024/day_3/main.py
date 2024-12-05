
from re import search
import os.path


mult_pat = r'((mul)\((\d+)\,(\d+)\)|(do)\(\)|(don\'t)\(\))'

star_1_total = 0
star_2_total = 0
total_length = 0
with open('input.txt', 'r') as f:
    full_string = f.read()
    inst = search(mult_pat, full_string)
    activated = True
    while inst:
        token = inst.group(2)
        if token: # mult
            fst_num = int(inst.group(3))
            snd_num = int(inst.group(4))
            star_1_total += fst_num * snd_num
            if activated:
                star_2_total += fst_num * snd_num
        token = inst.group(5)
        if token: # do
            activated = True
        token = inst.group(6)
        if token: # don't
            activated = False
        print(full_string[:inst.end()])
        print("   ", star_2_total, activated, inst.groups())
        full_string = full_string[inst.end():]
        total_length += inst.end()
        inst = search(mult_pat, full_string)
    total_length += len(full_string)

print('Star 1:', star_1_total)
print('Star 2:', star_2_total)
print('Length:', total_length, os.path.getsize('input.txt'))