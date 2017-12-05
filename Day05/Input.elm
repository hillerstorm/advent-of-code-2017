module Day05.Input exposing (parsedInput)


rawInput : String
rawInput =
    """0
2
0
-1
0
-4
-5
-5
-4
1
-6
-10
-9
-1
-1
1
-15
-15
-13
1
-2
-13
-6
-22
-10
-15
-3
-19
1
-26
-18
-13
-15
-15
-10
-4
0
-35
-4
-37
-29
-30
-38
-38
-13
-36
-42
-5
-28
-17
-34
-41
0
-41
-36
-46
-7
-51
-49
-47
-45
-30
-58
-33
-22
-38
-49
-37
-44
-53
-18
-66
-46
-47
-58
-22
-34
-41
-13
-41
-30
-34
-15
-38
-60
-61
-73
-20
-62
-48
-19
-40
-69
-86
-75
-9
-29
-2
-48
-96
-46
-89
-76
-34
-65
-38
-69
-5
-12
-54
-72
-87
-23
-82
-12
-24
-16
-115
-83
-3
-109
-72
-42
0
-48
-9
-34
-67
-83
-20
-33
-76
-81
0
-16
-106
-58
-91
-102
-123
-135
-85
-109
-61
-70
-103
-43
-104
-119
-75
-129
-104
-87
-95
-63
-1
-118
-49
-71
-34
-129
-52
-103
-98
-132
-119
-50
-36
-35
-24
-98
-139
-58
-25
-93
-82
-87
1
-14
-109
-89
-25
-96
-60
-79
-5
-124
-62
-44
-98
-119
-189
-66
-121
-151
-4
-14
-16
-154
-39
-51
-127
-13
-129
-98
-28
-6
-174
-169
-139
-22
-4
-2
-48
-62
-58
-163
-169
-124
-104
-205
-211
-43
2
-135
-41
-88
-208
-28
-124
-172
-223
-76
-98
-146
-55
-209
-197
-134
-93
2
-227
-39
-235
-240
-206
-70
-65
-38
-175
-198
-80
-10
-246
-228
-23
-84
-177
-81
-119
-161
-246
-75
-72
-243
-78
-233
-50
-204
-7
-206
-220
-46
-249
-135
-130
-143
-42
-65
-52
-79
-112
-147
-273
-54
-88
-200
-227
-24
-166
-113
-189
-30
-174
-55
-107
-14
-144
-148
-46
-263
-225
-85
-14
0
-197
-10
-6
-93
-153
-302
-176
-182
-251
-213
-9
-221
-111
-39
-134
-214
-155
-321
-212
-2
-207
-298
-124
-28
-78
-213
-194
-111
-159
-171
-240
-175
-99
-63
-162
-115
-147
-265
-153
-325
-19
-134
-49
-240
-322
-79
-61
-66
-127
-292
-282
-49
-114
-89
-16
-353
-181
-151
-72
-290
-313
-279
-351
-111
-220
-172
-98
-28
-223
-58
-51
-194
-138
-143
-308
-123
-28
-347
-87
-115
-295
-148
-116
-108
-267
-51
-346
-215
-44
-379
-309
-237
0
-212
-119
-231
-140
-270
-91
-146
-245
-232
-119
-131
-398
-264
-181
-303
-186
-404
-280
-412
-375
-292
-251
-138
-36
-18
-217
-117
-56
-272
-312
-160
-70
-130
-16
-279
-159
-6
-268
-283
-259
-197
-378
-24
-45
2
-390
-50
-246
-233
-294
-231
-364
-316
-189
-231
-74
-288
-286
-25
-317
-371
-434
-249
-54
-151
-234
-95
-158
-335
-362
-28
-438
-103
-173
-332
-97
-444
-459
-255
-295
-26
-120
-2
-152
-432
-191
-63
-313
-465
-1
-228
-468
-331
-231
-123
-403
-479
-441
-19
-75
-264
-483
-371
-277
-343
-52
-160
-489
-182
-338
-461
-233
-459
-291
-54
-61
-352
-276
-206
-290
-456
-81
-14
-331
-385
-241
-149
-421
-24
-12
-297
-93
-412
-478
0
-219
-157
-328
-344
-367
-343
-123
-349
-441
-197
-317
-165
-329
-515
-74
-443
-197
-75
-52
-534
-330
-178
-509
-199
-502
-429
-362
-422
-555
-183
-221
-461
-338
-496
-28
-507
-276
-271
-511
-298
-426
-144
-112
-198
-496
-158
-350
-326
-219
-315
-394
-555
-10
-422
-420
-216
-386
-344
-374
-567
-15
-23
-434
-44
-346
-110
-561
-198
-505
-103
-374
-107
-298
-38
-26
-171
-235
-324
-427
-359
-130
-500
-31
-221
-402
-240
-283
-47
-20
-422
-453
-31
-470
-115
-97
-120
-41
-590
-437
-53
-563
-440
-254
-545
-256
-341
-325
-417
-9
2
-442
-370
-317
-404
-498
-340
-402
-506
-381
-484
-582
-274
-157
-325
-445
-200
-56
-324
-31
-448
-407
-460
-84
-44
-387
-515
-206
-617
-322
-168
-340
-553
-629
-407
-344
-166
-619
-313
-222
-139
-199
-93
-474
-246
-165
-503
-636
-40
-298
-629
-294
-73
-438
-628
-632
-464
-512
-496
-683
-406
-241
-41
-251
-95
-264
-565
-183
-256
-634
-436
-660
-256
-528
-405
-4
-184
-513
-338
-476
-393
-449
-373
-585
-197
-334
-165
-161
-559
-424
-203
-1
-234
-511
-562
-234
-324
-339
-422
-269
-399
-249
-61
-630
-648
-37
-190
-196
-478
-150
-264
-40
-409
-600
-253
-708
-130
-463
-568
-292
-10
-350
-280
-617
-25
-218
-310
-72
-484
-741
-701
-284
-654
-442
-679
-718
-360
-488
-563
-192
-282
-342
-368
-95
-213
-511
-767
-194
-216
-574
-496
-770
-145
-652
-203
-26
-74
-564
-533
-605
-236
-183
-170
-755
-98
-174
-478
-476
-194
-167
-439
-724
-605
-364
-213
-35
-67
-378
-452
-59
-340
-663
-762
-506
-650
-223
-785
-53
-32
-241
-214
-274
-602
-308
-182
-367
-351
-327
-157
-526
-424
-229
-66
-669
-571
-538
-240
-379
-528
-667
-401
-832
-524
-651
-91
-102
-27
-586
-128
-836
-35
-653
-809
-109
-70
-707
-387
-351
-41
-7
-149
-10
-614
-181
-560
-24
-257
-305
-303
-91
-848
-249
-401
-624
-265
-751
-752
-367
-554
-715
-419
-449
-570
-62
-568
-203
-341
-751
-657
-347
-751
-639
-742
-307
-861
-706
-487
-644
-612
-390
-474
-565
-174
-263
-377
-307
-383
-390
-484
-722
-806
-874
-247
-570
-221
-51
-215
-641
-534
-427
-277
-647
-912
-787
-834
-270
-607
-354
-593
-740
-25
-222
-500
-494
-940
-442
-592
-938
-904
-580
-20
-938
-671
-199
-677
-917
-903
-206
-411
-917
-424
-300
-889
-501
-100
-117
-315
-678
-664
-579
-749
-636
-949
-642
-968
-343
-628
-190
-700
-705
-339
-240
-216
-628
-917
-724
-481
-900
-74
-291
-234
-934
-642
-874
-594
-955
-951
-341
-463
-706
-735
-556
-681
-985
-285
-604
-44
-153
-14
-78
-958
-44
-338
-765
-787
-487
-441
-518
-772
-632
-70
-74
-630
-362
-533
-684
-328
-407
-193
-727
-230
-454
-141
-568
-802
-326
-725
-464
-880
-990
-34"""


parsedInput : List Int
parsedInput =
    parse rawInput


parse : String -> List Int
parse =
    List.filterMap (Result.toMaybe << String.toInt) << String.lines
