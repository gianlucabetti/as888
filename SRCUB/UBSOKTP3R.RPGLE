000100151125     H Option(*SrcStmt)
000200160401     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300151126
000400151125     **-----------------------------------------------------------------------**
000500160330     ** TRANSPALLET PESATORE - GESTIONE DATI RICEVUTI
000600151126     **-----------------------------------------------------------------------**
000700160324
000800160330     FFITR400F  O    E             DISK    usropn
000900160330     F                                     extfile(LibFileTR4)
001000160324
001100160324     D*------------------
001200160330     D* DS INPUT
001300160324     D*------------------
001400160330     D DSIN_TPP        DS
001500160324     D  §TPP_SEGNBRT                 18A
001600160330     D  §TPP_PESO_GR                  9A
001700160427     D  §TPP_IDDEVICE                30A
001800160427
001900160427     D*------------------
002000160427     D* DS INPUT
002100160427     D*------------------
002200160427     D DS_IDDEVICE     DS
002300160427     D  §IDDEVICE_FIL                 3A
002400160427     D  §IDDEVICE_ID                 27A
002500160324
002600160324     D*-----------
002700160324     D* VARIABILI D WRK
002800160324     D*-----------
002900160401     D pInNETA         S              8A
003000160330     D pInDati         S           1024A
003100160330     D pInSktAddr      S             15A
003200160324     D pInTypCtrl      S              3A
003300160324     D pOutEsito       S              1N
003400160330     D LibFileTR4      S             21A   inz
003500160330
003600160330
003700160330     D*------------------
003800160330     D* DS PER TIPO CONTROLLO 'TPP'
003900160330     D*------------------
004000160330     D TPP_fltr4DS     DS
004100160330     D  tr4ctl                 1      1
004200160330     D  tr4npl                 2      3
004300160330     D  tr4nfv                 4      8
004400160330     D  tr4tim                 9     22
004500160330     D  tr4ms1                23     27
004600160330     D  tr4msx                28     28
004700160330     D  tr4ms2                29     31
004800160330     D  tr4cod                32     36
004900160330     D  tr4020                37     38
005000160330     D  tr4f01                39     39
005100160330     D  tr4pic                40     44
005200160330     D  tr4f02                45     45
005300160330     D  tr4pgr                46     51
005400160330     D  tr4f03                52     52
005500160330     D  tr4pod                53     55
005600160330     D  tr4f04                56     56
005700160330     D  tr4ods                57     57
005800160330     D  tr4f05                58     58
005900160330     D  tr4pad                59     61
006000160330     D  tr4f06                62     62
006100160330     D  tr4dts                63     63
006200160330     D  tr4f07                64     64
006300160330     D  tr4ps1                65     65
006400160330     D  tr4pn1                66     67
006500160330     D  tr4pd1                68     97
006600160330     D  tr4f08                98     98
006700160330     D  tr4ps2                99     99
006800160330     D  tr4pn2               100    101
006900160330     D  tr4pd2               102    130
007000160330     D  tr4xxx               131    131
007100160330     D  tr4f09               132    132
007200160330     D  tr4ps3               133    133
007300160330     D  tr4pn3               134    135
007400160330     D  tr4pd3               136    165
007500160330     D  tr4f10               166    166
007600160330     D  tr4ps4               167    167
007700160330     D  tr4pn4               168    169
007800160330     D  tr4pd4               170    199
007900160330     D  tr4dm1               181    184
008000160330     D  tr4dm2               185    188
008100160330     D  tr4dm3               189    192
008200160330     D  tr4f11               200    200
008300160330     D  tr4pep               201    214
008400160330     D  tr4f12               215    215
008500160330     D  tr4pes               216    216
008600160330     D  tr4f13               217    217
008700160330     D  tr4pup               218    231
008800160330     D  tr4f14               232    232
008900160330     D  tr4pus               233    233
009000160330     D  tr4f15               234    234
009100160401     D  tr4crc               235    236
009200160323
009300151012
009400160324     C*
009500160324     C     *ENTRY        PLIST
009600160401     C                   parm                    pInNETA
009700160330     C                   parm                    pInDati
009800160330     C                   parm                    pInSktAddr
009900160324     C                   parm                    pInTypCtrl
010000160324     C                   parm                    pOutEsito
010100160324     C*
010200160324     C* Inizializzo subito i paramteri di output
010300160324     C                   clear                   pOutEsito
010400160330     C*
010500160330     C* Per "sicurezza" (ovvero solidità del server socket) monitoro tutto
010600160330     C                   monitor
010700160330     C*
010800160330     C* Reperisco la DATA e ORA corrente
010900160330     C                   time                    wn14             14 0
011000160330     C                   movel     wn14          ORACOR            6 0          *ORA
011100160330     C                   z-add     *zeros        DATCOR            8 0
011200160330     C                   eval      DATCOR = %dec(%date() : *ISO)
011300160330     C*
011400160401     C* Gestisco in relazione al sistema AS/400 corrente
011500160401     C                   if        pInNETA = 'AS888'
011600160401     C                   eval      LibFileTR4 = 'FILTRAPRD/FITR400F'
011700160330     C                   else
011800160401     C                   eval      LibFileTR4 = 'FILTRA201/FITR400F'
011900160330     C                   endif
012000160324     C*
012100160324     C* Applico il tipo controllo richiesto in input
012200160324     C                   select
012300160324     C                   when      pInTypCtrl = 'TPP'                           * Transpallet Pesat.
012400160330     C*
012500160330     C                   if        not %open(FITR400F)
012600160330     C                   open      FITR400F
012700160330     C                   endif
012800160330     C*
012900160330     C                   eval      DSIN_TPP = pInDati
013000160427     C                   eval      DS_IDDEVICE = §TPP_IDDEVICE
013100160330     C*
013200160330     C                   CLEAR                   TPP_fltr4DS
013300160330     C                   EVAL      tr4ctl = *zeros
013400160330     C                   EVAL      tr4npl = *zeros
013500160330     C                   EVAL      tr4nfv = *zeros
013600160330     C                   EVAL      tr4tim = %editc(DATCOR:'X') +
013700160330     C                                      %editc(ORACOR:'X')
013800160330     C                   EVAL      tr4ms1 = *zeros
013900160330     C                   EVAL      tr4msx = *blanks
014000160330     C                   EVAL      tr4ms2 = *blanks
014100160330     C                   EVAL      tr4cod = *zeros
014200160330     C                   EVAL      tr4f01 = *blanks
014300160330     C                   EVAL      tr4pic = *zeros
014400160330     C                   EVAL      tr4f02 = *blanks
014500160330     C                   EVAL      tr4pgr = %subst(§TPP_PESO_GR:4:6)
014600160330     C                   EVAL      tr4f03 = *blanks
014700160505     C                   EVAL      tr4pod = '001'
014800160330     C                   EVAL      tr4f04 = *blanks
014900160330     C                   EVAL      tr4f05 = *blanks
015000160505     C                   EVAL      tr4pad = '001'
015100160330     C                   EVAL      tr4f06 = *blanks
015200160330     C                   EVAL      tr4dts = *zeros
015300160330     C                   EVAL      tr4f07 = *blanks
015400160330     C                   EVAL      tr4ps1 = *zeros
015500160330     C                   EVAL      tr4pn1 = *zeros
015600160502     C                   EVALR     tr4pd1 = '#' + §TPP_SEGNBRT + '#'
015700160330     C                   EVAL      tr4f08 = *blanks
015800160330     C                   EVAL      tr4ps2 = *zeros
015900160330     C                   EVAL      tr4pn2 = *zeros
016000160330     C                   EVAL      tr4pd2 = *blanks
016100160330     C                   EVAL      tr4pd4 = %trim(pInSktAddr)
016200160330     C                   EVAL      tr4f09 = *blanks
016300160330     C                   EVAL      tr4ps3 = *zeros
016400160330     C                   EVAL      tr4pn3 = *zeros
016500160427     C                   EVAL      tr4pd3 = §IDDEVICE_ID
016600160427     C                   EVAL      tr4f10 = *blanks
016700160427     C                   EVAL      tr4ps4 = *zeros
016800160330     C                   EVAL      tr4pn4 = *zeros
016900160330     C                   EVAL      tr4f11 = *blanks
017000160330     C                   EVAL      tr4020 = '2T'
017100160330     C                   EVAL      tr4f12 = *blanks
017200160512     C                   EVALR     tr4pep = 'TPPES'
017300160330     C                   EVAL      tr4pes = '1'
017400160330     C                   EVAL      tr4f13 = *blanks
017500160330     C                   EVAL      tr4pup = *blanks
017600160330     C                   EVAL      tr4f14 = *blanks
017700160330     C                   EVAL      tr4pus = '1'
017800160330     C                   EVAL      tr4ods = '1'
017900160330     C                   EVAL      tr4f15 = *blanks
018000160330     C                   EVAL      tr4crc = *blanks
018100160324     C*
018200160330     C* Scarico il dato del messaggio socket ricevuto e valorizzo il buffer del FITR400F
018300160330     C                   clear                   FITR4
018400160427     C                   eval      TR4FIL    = §IDDEVICE_FIL
018500160330     C                   eval      TR4PC     = *blanks
018600160330     C                   eval      TR4DA1    = TPP_fltr4DS
018700160330     C                   eval      TR4DATORA = %editc(DATCOR:'X') +
018800160330     C                                         %editc(ORACOR:'X')
018900160330     C                   write     FITR4
019000160330     C*
019100160330     C                   if        %open(FITR400F)
019200160330     C                   close     FITR400F
019300160330     C                   endif
019400160330     C*
019500160324     C                   endsl
019600160330     C                   eval      pOutEsito = *on
019700160324     C*
019800160330     C                   on-error
019900160330     C*
020000160330     C                   endmon
020100160330     C*
020200160324     C                   seton                                        LR
020300160324     C***
020400160324
