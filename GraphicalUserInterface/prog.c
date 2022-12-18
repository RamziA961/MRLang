#include <stdio.h>
int main(){
	int x = 0;
	if (x == 0) {
		x = x + 1;
		if (x == 1) {
			x = x + 1;
		};
	}
	else if (x == 1) {
		x = x + 2;
	}
	else {
		x = x + 3;
	};

	printf("%d", x);
	int y = 0;
}