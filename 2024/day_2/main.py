


reports = []

with open('input.txt') as f:
    for row in f:
        reports.append([int(i) for i in row.split(' ')])

def is_good(levels):
    diffs = [levels[i] - levels[i-1] for i in range(1, len(levels))]
    ascendings = [d > 0 for d in diffs]
    in_ranges = [1 <= abs(d) < 4 for d in diffs]
    ascending = sum(ascendings) >= (len(diffs) / 2)

    goods = [(ascending == a) and r for a, r in zip(ascendings, in_ranges)]
    return sum(goods) == len(goods)


part_1_safe = 0
part_2_safe = 0
for levels in reports:
    if is_good(levels):
        part_1_safe += 1
        part_2_safe += 1
    else:
        for i in range(len(levels)):
            if is_good(levels[:i] + levels[i+1:]):
                part_2_safe += 1
                break

    
    
    

print('Star 1:', part_1_safe)
print('Star 2:', part_2_safe)
