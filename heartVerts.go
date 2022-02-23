package main
var heartVerts []float32 = []float32{
	0.1719,0.6799,-0.0000,0.3, 0.3, 0.5,
	0.2047,4.0331,0.6211,0.3, 0.3, 0.5,
	-1.6218,4.0330,-0.0000,0.3, 0.3, 0.5,
	-1.5551,7.3272,1.1633,0.3, 0.3, 0.5,
	-3.8622,7.1088,-0.0000,0.3, 0.3, 0.5,
	4.2061,7.1088,-0.0000,0.3, 0.3, 0.5,
	1.3202,6.5464,1.0223,0.3, 0.3, 0.5,
	1.9655,4.0332,-0.0000,0.3, 0.3, 0.5,
	-8.5942,15.3266,1.6020,0.3, 0.3, 0.5,
	-8.9823,12.7360,-0.0000,0.3, 0.3, 0.5,
	-6.3290,13.4233,1.3298,0.3, 0.3, 0.5,
	-13.3216,21.7563,2.4526,0.3, 0.3, 0.5,
	-14.1139,18.3550,0.0000,0.3, 0.3, 0.5,
	-11.4378,19.2111,2.5162,0.3, 0.3, 0.5,
	-10.7727,21.5321,3.3499,0.3, 0.3, 0.5,
	-16.2812,21.4811,0.0000,0.3, 0.3, 0.5,
	-13.5604,24.0041,3.5972,0.3, 0.3, 0.5,
	-16.4494,32.1563,0.0000,0.3, 0.3, 0.5,
	-16.0001,28.2947,2.3901,0.3, 0.3, 0.5,
	-13.3629,30.3987,3.5059,0.3, 0.3, 0.5,
	-9.5697,25.3278,4.7331,0.3, 0.3, 0.5,
	-13.0434,25.8755,4.1248,0.3, 0.3, 0.5,
	-8.9380,33.7545,2.5265,0.3, 0.3, 0.5,
	-13.5489,32.4372,2.5823,0.3, 0.3, 0.5,
	-10.5398,30.4641,4.2654,0.3, 0.3, 0.5,
	-6.2620,35.1349,0.0000,0.3, 0.3, 0.5,
	-7.4248,32.6021,3.0010,0.3, 0.3, 0.5,
	-1.5800,29.5937,2.6901,0.3, 0.3, 0.5,
	0.1658,31.2204,0.0000,0.3, 0.3, 0.5,
	-2.7967,33.5877,0.0000,0.3, 0.3, 0.5,
	-7.7099,21.5875,3.9997,0.3, 0.3, 0.5,
	-4.9853,21.5301,4.0721,0.3, 0.3, 0.5,
	-6.8841,24.6034,4.7661,0.3, 0.3, 0.5,
	3.0384,29.9266,3.5740,0.3, 0.3, 0.5,
	3.1363,33.5667,0.0000,0.3, 0.3, 0.5,
	4.9619,31.8044,2.8114,0.3, 0.3, 0.5,
	6.0256,28.9464,4.3975,0.3, 0.3, 0.5,
	13.9988,34.6884,0.0000,0.3, 0.3, 0.5,
	10.3510,35.6519,0.0000,0.3, 0.3, 0.5,
	10.4938,33.4978,2.4984,0.3, 0.3, 0.5,
	10.0121,29.4685,4.5253,0.3, 0.3, 0.5,
	8.1497,31.9198,3.5050,0.3, 0.3, 0.5,
	16.6250,21.4812,0.0000,0.3, 0.3, 0.5,
	13.9548,21.9582,2.2720,0.3, 0.3, 0.5,
	14.4578,18.3549,0.0000,0.3, 0.3, 0.5,
	10.7816,25.9024,4.6995,0.3, 0.3, 0.5,
	8.9496,27.6062,4.7411,0.3, 0.3, 0.5,
	11.9689,15.4758,0.0000,0.3, 0.3, 0.5,
	8.9444,15.7181,1.7560,0.3, 0.3, 0.5,
	9.3257,12.7365,-0.0000,0.3, 0.3, 0.5,
	7.0253,18.1636,3.0492,0.3, 0.3, 0.5,
	10.8085,18.4770,2.1319,0.3, 0.3, 0.5,
	9.0081,20.6154,3.6017,0.3, 0.3, 0.5,
	12.0200,20.6773,2.5450,0.3, 0.3, 0.5,
	10.7107,23.7745,4.1935,0.3, 0.3, 0.5,
	2.9321,13.4993,2.2987,0.3, 0.3, 0.5,
	-0.5885,12.8675,2.1469,0.3, 0.3, 0.5,
	0.4780,9.9474,1.5589,0.3, 0.3, 0.5,
	-2.9547,9.9929,1.2578,0.3, 0.3, 0.5,
	-6.1189,15.8436,2.4404,0.3, 0.3, 0.5,
	-10.0122,35.6646,0.0000,0.3, 0.3, 0.5,
	12.9805,32.9216,2.3929,0.3, 0.3, 0.5,
	3.1487,17.5960,3.1868,0.3, 0.3, 0.5,
	2.5848,20.6730,3.9909,0.3, 0.3, 0.5,
	-0.8622,19.1034,3.6878,0.3, 0.3, 0.5,
	3.6838,9.5219,0.9863,0.3, 0.3, 0.5,
	5.8642,12.5787,1.4173,0.3, 0.3, 0.5,
	-6.3234,10.0119,-0.0000,0.3, 0.3, 0.5,
	-3.9725,12.1032,1.4432,0.3, 0.3, 0.5,
	-3.4681,17.4085,3.2500,0.3, 0.3, 0.5,
	-3.5161,14.5746,2.3316,0.3, 0.3, 0.5,
	-11.6251,15.4757,0.0000,0.3, 0.3, 0.5,
	-9.1266,17.9369,2.3038,0.3, 0.3, 0.5,
	-0.5051,16.0087,2.8383,0.3, 0.3, 0.5,
	-5.8072,18.7162,3.3782,0.3, 0.3, 0.5,
	-9.0668,29.1344,4.6024,0.3, 0.3, 0.5,
	-5.6096,29.7167,4.1061,0.3, 0.3, 0.5,
	-17.7899,24.9632,0.0000,0.3, 0.3, 0.5,
	-17.9905,28.7236,0.0000,0.3, 0.3, 0.5,
	-11.0509,27.2225,4.6746,0.3, 0.3, 0.5,
	-13.6575,34.6913,0.0000,0.3, 0.3, 0.5,
	-5.2770,31.9688,3.1264,0.3, 0.3, 0.5,
	0.1445,27.8440,3.4261,0.3, 0.3, 0.5,
	6.6017,35.1147,0.0000,0.3, 0.3, 0.5,
	0.1468,23.9258,4.4829,0.3, 0.3, 0.5,
	-2.4094,22.8625,4.4789,0.3, 0.3, 0.5,
	0.1040,21.5446,4.0777,0.3, 0.3, 0.5,
	3.2604,26.8078,4.3172,0.3, 0.3, 0.5,
	6.8462,24.9929,4.7661,0.3, 0.3, 0.5,
	16.7930,32.1564,0.0000,0.3, 0.3, 0.5,
	16.0214,29.9199,2.6734,0.3, 0.3, 0.5,
	18.3341,28.7238,0.0000,0.3, 0.3, 0.5,
	12.9371,28.8794,4.2084,0.3, 0.3, 0.5,
	15.8986,25.1080,2.5679,0.3, 0.3, 0.5,
	18.1337,24.9633,0.0000,0.3, 0.3, 0.5,
	13.2057,26.6299,4.2570,0.3, 0.3, 0.5,
	7.3663,14.1499,1.6033,0.3, 0.3, 0.5,
	5.7647,20.3949,3.9107,0.3, 0.3, 0.5,
	7.8884,22.5260,4.3579,0.3, 0.3, 0.5,
	6.6677,10.0115,-0.0000,0.3, 0.3, 0.5,
	3.9570,22.9186,4.4028,0.3, 0.3, 0.5,
	5.7782,15.5441,2.3143,0.3, 0.3, 0.5,
	-4.6029,26.6080,4.6084,0.3, 0.3, 0.5,
	-1.8295,25.5038,4.4597,0.3, 0.3, 0.5,
	0.2047,4.0331,-0.6211,0.3, 0.3, 0.5,
	-1.5551,7.3272,-1.1633,0.3, 0.3, 0.5,
	1.3202,6.5464,-1.0223,0.3, 0.3, 0.5,
	-8.5942,15.3266,-1.6020,0.3, 0.3, 0.5,
	-6.3290,13.4233,-1.3298,0.3, 0.3, 0.5,
	-13.3216,21.7563,-2.4526,0.3, 0.3, 0.5,
	-11.4378,19.2111,-2.5162,0.3, 0.3, 0.5,
	-10.7727,21.5321,-3.3499,0.3, 0.3, 0.5,
	-13.5604,24.0041,-3.5972,0.3, 0.3, 0.5,
	-13.3629,30.3987,-3.5059,0.3, 0.3, 0.5,
	-16.0001,28.2947,-2.3901,0.3, 0.3, 0.5,
	-9.5697,25.3278,-4.7331,0.3, 0.3, 0.5,
	-13.0434,25.8755,-4.1248,0.3, 0.3, 0.5,
	-8.9380,33.7545,-2.5265,0.3, 0.3, 0.5,
	-10.5398,30.4641,-4.2653,0.3, 0.3, 0.5,
	-13.5489,32.4372,-2.5823,0.3, 0.3, 0.5,
	-7.4248,32.6021,-3.0010,0.3, 0.3, 0.5,
	-1.5800,29.5937,-2.6901,0.3, 0.3, 0.5,
	-7.7099,21.5875,-3.9997,0.3, 0.3, 0.5,
	-6.8841,24.6034,-4.7661,0.3, 0.3, 0.5,
	-4.9853,21.5301,-4.0721,0.3, 0.3, 0.5,
	3.0384,29.9266,-3.5740,0.3, 0.3, 0.5,
	4.9619,31.8044,-2.8114,0.3, 0.3, 0.5,
	6.0256,28.9464,-4.3975,0.3, 0.3, 0.5,
	10.4938,33.4978,-2.4984,0.3, 0.3, 0.5,
	10.0121,29.4685,-4.5253,0.3, 0.3, 0.5,
	8.1497,31.9198,-3.5050,0.3, 0.3, 0.5,
	13.9548,21.9582,-2.2720,0.3, 0.3, 0.5,
	10.7816,25.9024,-4.6995,0.3, 0.3, 0.5,
	8.9496,27.6062,-4.7411,0.3, 0.3, 0.5,
	8.9444,15.7181,-1.7560,0.3, 0.3, 0.5,
	7.0253,18.1636,-3.0492,0.3, 0.3, 0.5,
	10.8085,18.4770,-2.1319,0.3, 0.3, 0.5,
	9.0081,20.6154,-3.6017,0.3, 0.3, 0.5,
	10.7107,23.7745,-4.1935,0.3, 0.3, 0.5,
	12.0200,20.6773,-2.5450,0.3, 0.3, 0.5,
	2.9321,13.4993,-2.2987,0.3, 0.3, 0.5,
	0.4780,9.9474,-1.5589,0.3, 0.3, 0.5,
	-0.5885,12.8675,-2.1469,0.3, 0.3, 0.5,
	-2.9547,9.9929,-1.2578,0.3, 0.3, 0.5,
	-6.1189,15.8436,-2.4404,0.3, 0.3, 0.5,
	12.9805,32.9216,-2.3929,0.3, 0.3, 0.5,
	3.1487,17.5960,-3.1868,0.3, 0.3, 0.5,
	-0.8622,19.1034,-3.6878,0.3, 0.3, 0.5,
	2.5848,20.6730,-3.9909,0.3, 0.3, 0.5,
	3.6838,9.5219,-0.9863,0.3, 0.3, 0.5,
	5.8642,12.5787,-1.4173,0.3, 0.3, 0.5,
	-3.9725,12.1032,-1.4432,0.3, 0.3, 0.5,
	-3.4681,17.4085,-3.2500,0.3, 0.3, 0.5,
	-3.5161,14.5746,-2.3316,0.3, 0.3, 0.5,
	-9.1266,17.9369,-2.3038,0.3, 0.3, 0.5,
	-0.5051,16.0087,-2.8383,0.3, 0.3, 0.5,
	-5.8072,18.7162,-3.3782,0.3, 0.3, 0.5,
	-5.6096,29.7167,-4.1061,0.3, 0.3, 0.5,
	-9.0668,29.1344,-4.6024,0.3, 0.3, 0.5,
	-11.0509,27.2225,-4.6746,0.3, 0.3, 0.5,
	-5.2770,31.9688,-3.1264,0.3, 0.3, 0.5,
	0.1445,27.8440,-3.4261,0.3, 0.3, 0.5,
	0.1468,23.9258,-4.4829,0.3, 0.3, 0.5,
	0.1040,21.5446,-4.0777,0.3, 0.3, 0.5,
	-2.4094,22.8625,-4.4789,0.3, 0.3, 0.5,
	6.8462,24.9929,-4.7661,0.3, 0.3, 0.5,
	3.2604,26.8078,-4.3172,0.3, 0.3, 0.5,
	16.0214,29.9199,-2.6734,0.3, 0.3, 0.5,
	12.9371,28.8794,-4.2084,0.3, 0.3, 0.5,
	15.8986,25.1080,-2.5679,0.3, 0.3, 0.5,
	13.2057,26.6299,-4.2570,0.3, 0.3, 0.5,
	7.3663,14.1499,-1.6033,0.3, 0.3, 0.5,
	5.7647,20.3949,-3.9107,0.3, 0.3, 0.5,
	7.8884,22.5260,-4.3579,0.3, 0.3, 0.5,
	3.9570,22.9186,-4.4028,0.3, 0.3, 0.5,
	5.7782,15.5441,-2.3143,0.3, 0.3, 0.5,
	-4.6029,26.6080,-4.6084,0.3, 0.3, 0.5,
	-1.8295,25.5038,-4.4597,0.3, 0.3, 0.5,
}
var heartIndices []uint16 = []uint16{
	0,1,2,
	2,3,4,
	5,6,7,
	8,9,10,
	11,12,13,
	11,13,14,
	15,11,16,
	17,18,19,
	20,21,16,
	22,23,24,
	25,22,26,
	27,28,29,
	30,31,32,
	28,33,34,
	35,33,36,
	37,38,39,
	40,41,36,
	42,43,44,
	45,40,46,
	47,48,49,
	50,48,51,
	52,53,54,
	55,56,57,
	58,3,57,
	3,2,1,
	8,10,59,
	22,60,23,
	61,39,40,
	62,63,64,
	65,66,55,
	67,4,58,
	10,9,67,
	56,58,57,
	55,57,65,
	58,4,3,
	6,65,57,
	6,57,3,
	58,56,68,
	68,67,58,
	69,64,31,
	10,68,70,
	71,9,8,
	13,12,71,
	59,72,8,
	73,64,69,
	10,70,59,
	10,67,68,
	59,74,72,
	72,71,8,
	26,75,76,
	30,14,72,
	15,12,11,
	15,16,77,
	14,16,11,
	30,32,20,
	72,14,13,
	13,71,72,
	18,78,77,
	14,20,16,
	77,21,18,
	17,78,18,
	77,16,21,
	17,19,23,
	79,19,21,
	19,18,21,
	80,17,23,
	23,60,80,
	24,23,19,
	79,24,19,
	25,60,22,
	81,29,25,
	27,29,81,
	22,24,26,
	33,28,82,
	75,79,20,
	26,76,81,
	81,25,26,
	32,75,20,
	27,81,76,
	30,72,74,
	30,20,14,
	35,36,41,
	52,51,53,
	35,41,83,
	84,85,86,
	83,39,38,
	37,39,61,
	39,41,40,
	36,87,88,
	89,37,61,
	89,90,91,
	61,40,92,
	83,41,39,
	90,89,61,
	93,94,91,
	90,92,95,
	36,88,46,
	90,95,93,
	42,94,93,
	43,42,93,
	46,88,45,
	51,47,44,
	95,54,43,
	45,88,54,
	54,95,45,
	43,54,53,
	44,43,53,
	48,96,49,
	62,64,73,
	49,96,66,
	50,51,52,
	97,52,98,
	54,88,98,
	5,99,65,
	6,5,65,
	64,85,31,
	98,88,100,
	62,97,63,
	101,96,48,
	73,55,62,
	70,68,56,
	73,69,70,
	102,32,31,
	73,56,55,
	62,101,50,
	27,76,102,
	33,82,87,
	82,103,84,
	64,63,86,
	59,69,74,
	70,56,73,
	59,70,69,
	69,31,74,
	79,75,24,
	21,20,79,
	36,33,87,
	98,100,97,
	82,84,87,
	27,102,103,
	82,28,27,
	27,103,82,
	34,33,35,
	35,83,34,
	97,50,52,
	52,54,98,
	51,44,53,
	51,48,47,
	100,88,87,
	86,63,84,
	45,92,40,
	40,36,46,
	90,93,91,
	90,61,92,
	95,43,93,
	92,45,95,
	49,66,99,
	101,62,55,
	65,99,66,
	66,101,55,
	62,50,97,
	63,100,84,
	30,74,31,
	102,31,85,
	63,97,100,
	100,87,84,
	84,103,85,
	64,86,85,
	101,48,50,
	66,96,101,
	102,85,103,
	75,32,102,
	26,24,75,
	75,102,76,
	0,7,1,
	1,6,3,
	1,7,6,
	0,2,104,
	2,4,105,
	5,7,106,
	107,108,9,
	109,110,12,
	109,111,110,
	15,112,109,
	17,113,114,
	115,112,116,
	117,118,119,
	25,120,117,
	121,29,28,
	122,123,124,
	28,34,125,
	126,127,125,
	37,128,38,
	129,127,130,
	42,44,131,
	132,133,129,
	47,49,134,
	135,136,134,
	137,138,139,
	140,141,142,
	143,141,105,
	105,104,2,
	107,144,108,
	117,119,60,
	145,129,128,
	146,147,148,
	149,140,150,
	67,143,4,
	108,67,9,
	142,141,143,
	140,149,141,
	143,105,4,
	106,141,149,
	106,105,141,
	143,151,142,
	151,143,67,
	152,124,147,
	108,153,151,
	71,107,9,
	110,71,12,
	144,107,154,
	155,152,147,
	108,144,153,
	108,151,67,
	144,154,156,
	154,107,71,
	120,157,158,
	122,154,111,
	15,109,12,
	15,77,112,
	111,109,112,
	122,115,123,
	154,110,111,
	110,154,71,
	114,77,78,
	111,112,115,
	77,114,116,
	17,114,78,
	77,116,112,
	17,119,113,
	159,116,113,
	113,116,114,
	80,119,17,
	119,80,60,
	118,113,119,
	159,113,118,
	25,117,60,
	160,25,29,
	121,160,29,
	117,120,118,
	125,161,28,
	158,115,159,
	120,160,157,
	160,120,25,
	123,115,158,
	121,157,160,
	122,156,154,
	122,111,115,
	126,130,127,
	137,139,136,
	126,83,130,
	162,163,164,
	83,38,128,
	37,145,128,
	128,129,130,
	127,165,166,
	89,145,37,
	89,91,167,
	145,168,129,
	83,128,130,
	167,145,89,
	169,91,94,
	167,170,168,
	127,133,165,
	167,169,170,
	42,169,94,
	131,169,42,
	133,132,165,
	136,44,47,
	170,131,138,
	132,138,165,
	138,132,170,
	131,139,138,
	44,139,131,
	134,49,171,
	146,155,147,
	49,150,171,
	135,137,136,
	172,173,137,
	138,173,165,
	5,149,99,
	106,149,5,
	147,124,164,
	173,174,165,
	146,148,172,
	175,134,171,
	155,146,140,
	153,142,151,
	155,153,152,
	176,124,123,
	155,140,142,
	146,135,175,
	121,176,157,
	125,166,161,
	161,162,177,
	147,163,148,
	144,156,152,
	153,155,142,
	144,152,153,
	152,156,124,
	159,118,158,
	116,159,115,
	127,166,125,
	173,172,174,
	161,166,162,
	121,177,176,
	161,121,28,
	121,161,177,
	34,126,125,
	126,34,83,
	172,137,135,
	137,173,138,
	136,139,44,
	136,47,134,
	174,166,165,
	163,162,148,
	132,129,168,
	129,133,127,
	167,91,169,
	167,168,145,
	170,169,131,
	168,170,132,
	49,99,150,
	175,140,146,
	149,150,99,
	150,140,175,
	146,172,135,
	148,162,174,
	122,124,156,
	176,164,124,
	148,174,172,
	174,162,166,
	162,164,177,
	147,164,163,
	175,135,134,
	150,175,171,
	176,177,164,
	158,176,123,
	120,158,118,
	158,157,176,
	0,104,7,
	104,105,106,
	104,106,7,
}
