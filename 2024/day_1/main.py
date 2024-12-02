
left_list = []
right_list = []

with open('input.txt', 'r') as f:
    for row in f:
        [left, right] = row.split('   ')
        left_list.append(int(left))
        right_list.append(int(right))

assert len(left_list) == len(right_list)

left_list = sorted(left_list)
right_list = sorted(right_list)

sum_diff = 0
for left, right in zip(left_list, right_list):
    sum_diff += abs(left - right)

print('Star 1:', sum_diff)

i, j = 0, 0

part_2_sum = 0

while i < len(left_list) and j < len(right_list):
    left = left_list[i]
    right = right_list[j]

    if left == right:
        part_2_sum += left
        j += 1
    elif left < right:
        i += 1
    else:
        j += 1

print('Star 2:', part_2_sum)