int hello (int y,int z) {
	const int i = 0;
}

int main () {
	int i = 99;
	int j = 99;
	if (i == j) {
		j = j + i;
		i = 2 * i + j + 300;
	}
	else {
		i = j + i;
		j = 2 * j + i / 200;
	};
}