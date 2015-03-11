#include <stdlib.h>
#include <stdio.h>

int
work(long *nums, long len, int a, int b, int c, int d)
{
	if (b >= len)
		return c;
	if (!nums[a] + nums[b])
		return work(nums, len, a+2, b+2, c+d+1, 1);
	else
		return work(nums, len, a+1, b+1, c, 0);
}

int
main()
{
	char *line = NULL;
	size_t size = 0;
	long *nums, num, i = 0;

	getline(&line, &size, stdin);

	num = strtol(line, NULL, 10);

	nums = malloc(sizeof(long) * num);
	if (!nums)
		return 1;

	while(getline(&line, &size, stdin) != -1) {
		nums[i++] = strtol(line, NULL, 10);
	}

	printf("%d\n", work(nums, num, 0, 1, 0, 0));

	return 0;
}
