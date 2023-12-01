#include <iostream>
#include <string>
#include <vector>
#include <format>
using namespace std;

const string NUMBERS = "1234567890";
const string NUMBER_NAMES[] = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

int getNum(string line, bool lookForString, bool first) {

    size_t num_idx = first ? line.find_first_of(NUMBERS) : line.find_last_of(NUMBERS);
    int num_num = line[num_idx] - '0';

    if (lookForString) {
        for (int i=0; i < 10; i++) {
            size_t idx = first ? line.find(NUMBER_NAMES[i]) : line.rfind(NUMBER_NAMES[i]);


            // whoo boy logic spaghetti
            // Not enough brain cells to prove that the last two statements are equivalent
            // Quick test says they are not equivalent, who knew!
            if (idx != string::npos && ((first && idx < num_idx) || (!first && idx > num_idx))) {
                num_idx = idx;
                num_num = i;
            }
        }
    }
    return num_num;
}

int getCalibration(bool lookForString) {
    int sum = 0;
    for (string line; getline(cin, line);) {
        int first_num = getNum(line, lookForString, true);
        int last_num = getNum(line, lookForString, false);

        string num_str = "00"; // crude way to do it, but it works
        num_str[0] = first_num + '0';
        num_str[1] = last_num + '0';

        int num = stoi(num_str);
        sum += num;
    }
    return sum;
}

int main (int argc, char ** argv) {
    bool isPartOne = false;
    if (argc == 2 && strcmp(argv[1], "--part-1") == 0) {
        isPartOne = true;
    } else if (argc == 2 && strcmp(argv[1], "--part-2") == 0) {
        isPartOne = false;
    } else {
        cout << "Advent of code day 1" << endl;
        cout << "Usage: ./main (--part-1|--part-2) Run either part one or part two" << endl;
        return 0;
    }

    int sum = getCalibration(!isPartOne);
    cout << sum << endl;
    return 0;
}
