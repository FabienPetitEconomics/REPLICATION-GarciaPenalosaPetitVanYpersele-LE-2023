** STANDARD RECODE OF OCCUPATIONS IN EGP SCORE
* THE MODULE HAS BEEN CHANGED TO A FORMAT FOR AN INDETERMINATE
* NUMBER OF VARIABLES. YOU NEED TO DEFINE IN YOUR FILE THE FOLLOWING
* MACRO VARIABLES:
* @ISCO
* @EGP10
* @SEMPL
* @SUPVIS.
 
DO REPEAT      E=@EGP10 / IS=@ISCO.
COMPUTE        E=IS.
END REPEAT.

comment        **************** iscoegp*******************************
               last revision March 12 1993
               ********************************************************
               You have to define for each occupation:
               isco: four digit International Standard Classification
               sempl: (2) self employed
               supvis: (>10) large (1-10) small supervisor
               *******************************************************
               manual workers who are self-employed and have more than
               10 subordinates now end up in EGP 1
               **************** changes *******************************
               MARCH 3 1991: 1960=1 (GENERIC PROFESSIONALS)
               version august 20, 1988. Changes compared to recoding
               scheme for dutch mobility data:
               * self-employed farmers (ivc) will be scored as (11)
               * tracers (321) to 3 (from 9)
               * primary principal (1391) to 2 (from 1)
               * general managers (2110) to 2 (from 1)
               * expeditors/dispatchers (3590) to 3 (from 7)
               * conductors (3600-3601) in 3 (from 2)
               * sales engineers (4200-4222) to 2 (from 3)
               * other salesmen (4510-4514) to 3 (from 9)
               * market/street vendor (4520-4521) to 5 (from 9)
               * watchman (5890) to 9 (from 7)
               * prison guard (5891) to 9 (from 7)
               * entertainment attendant (5991) to 3 (from 9)
               * fishermen (6400-6490) to 10/11 (from 9)
               * miller (7710) into 9 (from 8)
               * woodmachine operators (8120) into 9 (from 8)
               * shoe and leather workers (8020-8030) into 9 (from 8)
               * dental mechanic (8422) electricians (8540-8550)
                 telephone installers (8560-8590) broadcast oprt (8610)
                 to 8 (from 7)
               * ceramic kilnmen (8930) and ceramic wrke nec (8990)
                 into 9 (from 8)
               * carpenters helper (9542) insulation wrkr (9560)
                 glazier (9570) to 9 (from 8)
               * crane-road mach oprtr (9730-9740) to 8 (from 9)
               * unskilled foreman are not promoted to 7
               * added revision for isco, that distinguishes between
                 2-, 3-, and 4-digits (4-digit categories that end on 0,
                 are moved to a code with 9 as last digit
               * as a consequence, 5999 is now a "medical attendant",
                 and "illegal lottery agent" is lost
               ********************************************************.
 
RECODE         @EGP10
               (100 thru 139=1)(140 149=2)(200 thru 299=1)
               (300 320 329=2)
               (310=2)(321=3)(330 thru 390=2)(400 thru 420 429=1)
               (421=2)(430=1)(500 510 thru 539=1)(540 thru 549=2)
               (600 thru 619=1)(620=2)(630=1)(640=2)(650=1)(660=2)
               (670=1)(680 690=2)(700=2)(710 thru 719=2)(720=3)
               (721 thru 761 769=2)(762=8)(770 thru 799=2)
               (800 thru 820=1)(830=2)(840 849=2)(900 thru 1290 1299=1)
               (1291=2)(1310 thru 1319=1)(1300 1320 thru 1389=2)
               (1390 1399=2)(1391=2)(1393=3)(1392 1394=1)
               (1400 1410 1412 thru 1490 1499=2)(1411=1)(1491=0)
               (1500 thru 1739=2)(1740=1)(1741 thru 1809=2)(1900=1)
               (1910=2)(1920 thru 1929=1)(1930 thru 1950 1959=2)
               (1960=1)
               (1951=1)(1990 thru 1999=2)(2000 thru 2039=1)
               (2100 thru 2110=2)(2119=1)(2112 2113=2)(2114 2115=1)
               (2190 2191 2196 2199=2)(2116 2120=2)(2111=1)
               (2192 thru 2195 2197=1) (2200 thru 3109=2)
               (3200 thru 3420=3)(3590=3)(3500 3510 3520=2)
               (3600 thru 3609=3)(3700 thru 3709=3)(3800 thru 3999=3)
               (4000 thru 4009=2)(4101 4103 4104 4106 4108=4)
               (4100 4102 4105 4109=5)(4107=0)(4200 4210=2)
               (4220 thru 4229=2)(4300 thru 4310 4319=2)(4311 4320=3)
               (4400 thru 4439=2)(4500 thru 4511 4519=3)(4512=9)
               (4513 4514=3)(4520 4521 4529=5)(4522 4523=3)(4524=9)
               (4525=0)(4900=3)(5000 THRU 5009=2)(5100 THRU 5109=4)
               (5200 thru 5209=3)(5300 thru 5311 5319=8)(5312=9)
               (5320 thru 5329=3)(5400 thru 5600=9)(5700 thru 5709=8)
               (5800 5810=8)(5820 5829=2)(5822=1)(5821 5823=2)
               (5890 thru 5899=9)(5900 thru 5920=3)
               (5990 5999=9)(5991=3)(5992 thru 5995=9)
               (5996 thru 5998=3)(6000 thru 6120=11)(6200 thru 6329=10)
               (6400 6410 6419=10)(6411=11)(6490 thru 6499=10)
               (7000 7001 7009=7)(7100 7110 7111 7113 7119=8)
               (7112 7120=9)(7130 thru 7280=8)(7290=9)(7300 thru 7329=8)
               (7330 thru 7340=9)(7400 thru 7520=9)(7530 thru 7549=8)
               (7550 thru 7560=8)(7570 thru 7590=9)(7600 thru 7699=8)
               (7700 thru 7710 7719=9)(7711=4)(7720 7730 7739=8)(7731=9)
               (7732=8)(7740 7750=9)(7760 thru 7780=8)(7790 thru 7890=9)
               (7900 thru 7940=8)(7950 thru 7970=9)(7980=8)(7990=9)
               (8000 thru 8010=8)(8020 8030=9)(8100 8110=8)(8120=9)
               (8190 thru 8339=8)(8340=9)(8350 thru 8399=8)
               (8400 thru 8490 8499=8)(8491 thru 8494=9)
               (8500 8510 8520=8)(8530=9)(8540 thru 8550 8559=8)(8551=4)
               (8560 thru 8610=8)(8620=3)(8700 8710 8719=8)(8711=4)
               (8720 thru 8920=8)(8930 thru 8990=9)(9000 thru 9100=9)
               (9200 thru 9299=8)(9300 thru 9319=8)(9390=9)(9400=9)
               (9410=8)(9430=9)(9420=9)(9430=8)(9490 thru 9492 9499=8)
               (9493=9)(9500 thru 9520=8)(9530=9)(9540 9541 9549=8)
               (9542=9)(9550 9551 9559=8)(9560 thru 9570=9)
               (9590 thru 9593 9599=8)(9594 9595=9)(9596=8)
               (9600 thru 9699=9)(9700 thru 9719=9)(9720=8)
               (9730 9739=8)(9731=9)(9740=8)(9790=9)(9800 thru 9820=9)
               (9830=9)(9839=8)(9831 thru 9854=9)(9855=3)(9859=9)
               (9860 thru 9900=9)(9950 9959=8)(9951=5)(9970 thru 9999=9)
               (10000=7)(10001=1)(10002=7)(10003=8)
               (5830=7)(5831=1)(5832=7)(5833=8)(lo thru 99=-1)
               (10004 thru hi=-1).
 
comment       *** ascertain self-employment on basis of isco title ***.
 
DO REPEAT      IS=@ISCO /S=@SEMPL/.
if             (is ge 4100 and is le 4108) s=2.
if             (is ge 6100 and is le 6112) s=2.
if             (is eq 6120) s=2.
if             (is eq 8551 or is eq 8711) s=2.
if             (is eq 4520 or is eq 4521) s=2.
if             (is eq 9951) s=2.
end repeat.
 
DO REPEAT      S=@SEMPL / E=@EGP10 / IS=@ISCO / SV=@SUPVIS.
COMPUTE        #X=IS.
comment       *** jobtitles are sorted into iva or ivb in advance ***.
RECODE         #X
               (421 3000 4000 thru 4002 4101 4106 4210 5000 5001 5002
               5100 5101 5103 7730 7760 9596 5103 5920=4).
               (1630 1631 3300 thru 3391 3310 3520 3700 4104
               4300 thru 4320 4400 4420 thru 4900 5300 thru 5999=5)
               (else=-1).
IF             ((#X=4 OR #X=5) AND S EQ 2) E=#X.
COMMENT        #P CODES PROMOTABILITY OF CERTAIN OCCUPATIONS.
COMPUTE        #P=IS.
RECODE         #P
               (410 thru 430 711 793 1391 1394 1591 2010 thru 2197 3000
               3100 thru 3104 3312 3500 thru 3590 4000 thru 4220
               4410 thru 4412 5000 thru 5201 5821 thru 5823=1)(else=-1).
IF             ((E GE 7 AND E LE 9) AND (S=2) AND (SV GE 1)) E=4.
IF             ((E GE 7 AND E LE 9) AND (S=2) AND (SV LE 0)) E=5.
IF             ((E=8) AND (SV GE 10)) E=7.
IF             ((E=10) AND (S=2)) E=11.
IF             ((E=4) AND (SV=0)) E=5.
IF             ((E=5) AND (SV GE 1)) E=4.
IF             (E=4 OR E=5) #P=1.
IF             ((E=2 OR E=3 OR E=4) AND (SV GE 10) AND (#P=1)) E=1.
IF             ((E=3) AND (SV GE 1 AND SV LT 10) AND (#P=1)) E=2.
end repeat.
 
** an attempt to separate routine nonmanuals **.

recode @egp10 (4=5)(5=6).
do repeat eee=@egp10 /iii=@isco.
do if (eee eq 3).
recode iii (720 3700 thru 3709 3919 4500 thru 4519 4522 4523
   4900 5320 thru 5329 5900 5910 5991=4) into eee.
end if.
end repeat.

value labels   @egp10 (1) Higher Controllers (2) Lo Controllers
               (3) Routine NonManual (4) Lo Service-Sales
               (5) Sempl with Empl
               (6) Sempl no Empl (7) Manual Supervis
               (8) Skilled Manual (9) Semi-Unskilld Manual
               (10) Farm Labor (11) Selfempl Farm.
