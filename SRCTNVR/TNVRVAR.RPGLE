000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300061026     Ffnarbm0f  iF   E             DISK
000400061026     Ffnarbd0f  iF   E             DISK
000500061026     Ffiar601l  iF   E           K DISK
000600061026     Ffiar401l  iF   E           K DISK
000700061026     Ffnblp01l  iF   E           K DISK
000800061026     Ffnarb01l  iF   E           K DISK    prefix(blp:3)
000900021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000021214      *--------------------------------------------------
001100050323     c*
001200061026     c* se bolla 8888 conto tutte le Mi e le I0
001300061026     c* se bolla 9999 conto solo se data variazione > data consegna
001400061026     c
001500061026     C                   read      fnarbm0f
001600061026    1c                   DOW       not %EOF(fnarbm0f)
001700061026     c
001800061026     c     karb          chain     fnarb01l                           30
001900061026     c   30karb          chain     fnblp01l
002000061026     c                   if        not *in30   and blpcbo<>'A ' and blplna
002100061026     c                             <>101 and blplna<>102
002200061026     c
002300061026     c                   move      blpksc        w0040             4 0
002400061026     c*
002500061026     c                   if        w0040=8888
002600061026     c* Se non cambiata la ragione sociale lo indico in stampa
002700061026     c                   add       1             conta88MI         9 0
002800061026     c                   seton                                        03
002900061026     c                   if        blprsm=arbrsm
003000061026     c                   seton                                        04
003100061026     c                   endif
003200061026     c
003300061026     c                   except    bolla
003400061026     c                   setoff                                       0304
003500061026     c                   endif
003600061026     c                   endif
003700061026     C                   read      fnarbm0f
003800061026     c                   enddo
003900061026     c
004000061026     c
004100061026     C                   read      fnarbd0f
004200061026    1c                   DOW       not %EOF(fnarbd0f)
004300061026     c
004400061026     c                   if        arbcvb='I0'
004500061026     c
004600061026     c     karb          chain     fnarb01l                           30
004700061026     c   30karb          chain     fnblp01l
004800061026     c                   if        not *in30
004900061026     c                   move      blpksc        w0040             4 0
005000061026     c                   movel     'D'           ktrc
005100061026     c     kar6          chain     fiar401l
005200061026     c                   if        not %found(fiar401l)
005300061026     c                   clear                   ar4not
005400061026     c                   endif
005500061026     c*
005600061026     c                   if        w0040=8888  and blpcbo<>'A'
005700061026     c                             and blplna<>101 and blplna<>102
005800061026     c* solo se modificato qualche cosa conto
005900061026     c                   if        blprsd<>arbrsd or
006000061026     c                             ar4not<>arbrd2 or
006100061026     c                             blpind<>arbind or
006200061026     c                             blpcad<>arbcad or
006300061026     c                             blplod<>arblod or
006400061026     c                             blpnzd<>arbnzd
006500061026     c                   add       1             conta88I0         9 0
006600061026     c* conto senza la seconda parte della ragione sociale
006700061026     c                   if        blprsd<>arbrsd or
006800061026     c                             blpind<>arbind or
006900061026     c                             blpcad<>arbcad or
007000061026     c                             blplod<>arblod or
007100061026     c                             blpnzd<>arbnzd
007200061026     c                   add       1             conta88I0parz     9 0
007300061026     c                   endif
007400061026     c
007500061026     c                   seton                                        01
007600061026     c                   if        blprsd=arbrsd
007700061026     c                   seton                                        04
007800061026     c                   endif
007900061026     c                   except    bolla
008000061026     c                   setoff                                       0104
008100061026     c                   endif
008200061026     c                   endif
008300061026     c*
008400061026     c                   if        w0040=9999 and arbdtv>blpdcm and
008500061026     c                             blpdcm>0
008600061026     c* controllo in fiar6 se diventato segue fattura o fatt imm
008700061026     c                   movel     '1'           ktrc
008800061026     c     kar6          chain     fiar601l
008900061026     c                   if        %found(fiar601l)
009000061026     c                   move      ar6ksc        w0040             4 0
009100061026     c                   endif
009200061026     c                   if        w0040=9999
009300061026     c                   if        blprsd<>arbrsd or
009400061026     c                             ar4not<>arbrd2 or
009500061026     c                             blpind<>arbind or
009600061026     c                             blpcad<>arbcad or
009700061026     c                             blplod<>arblod or
009800061026     c                             blpnzd<>arbnzd
009900061026     c                   add       1             conta99I0         9 0
010000061026     c
010100061026     c                   if        blprsd<>arbrsd or
010200061026     c                             blpind<>arbind or
010300061026     c                             blpcad<>arbcad or
010400061026     c                             blplod<>arblod or
010500061026     c                             blpnzd<>arbnzd
010600061026     c                   add       1             conta99I0parz     9 0
010700061026     c                   endif
010800061026     c
010900061026     c                   seton                                        02
011000061026     c                   if        blprsd=arbrsd
011100061026     c                   seton                                        04
011200061026     c                   endif
011300061026     c                   except    bolla
011400061026     c                   setoff                                       0204
011500061026     c                   endif
011600061026     c                   endif
011700061026     c                   endif
011800061026     c
011900061026     c* Verifico se bolla doppia  e seconda bolla con fattura immed
012000061026     c                   movel     '2'           ktrc
012100061026     c     kar6          chain     fiar601l
012200061026     c                   if        %found(fiar601l)
012300061026     c                   move      ar6ksc        w0040             4 0
012400061026     c                   if        w0040=9999 and arbdtv>blpdcm and
012500061026     c                             blpdcm>0
012600061026     c                   add       1             conta2bI0         9 0
012700061026     c                   endif
012800061026     c                   endif
012900061026     c
013000061026     c
013100061026     c                   endif
013200061026     c                   endif
013300061026     C                   read      fnarbd0f
013400061026     c                   enddo
013500061026     c
013600061026     c                   except    totali
013700021216      *
013800021214     C                   eval       *inlr = *on
013900021214      *---------------------------------------------------------
014000021214     c     *inzsr        BEGSR
014100021214      *
014200050118     C     Karb          KLIST
014300050404     C                   KFLD                    arbaas
014400050404     C                   KFLD                    arblnp
014500050404     C                   KFLD                    arbnrs
014600050404     C                   KFLD                    arbnsp
014700061026     C     Kar6          KLIST
014800061026     C                   KFLD                    arbaas
014900061026     C                   KFLD                    arblnp
015000061026     C                   KFLD                    arbnrs
015100061026     C                   KFLD                    arbnsp
015200061026     C                   KFLD                    ktrc              1
015300021216     C*
015400021214     C                   ENDSR
015500021214      *---------------------------------------------------------
015600061026     OQSYSPRT   E            totali      1  1
015700061026     O                                              'Variazioni MI per'
015800061026     O                                         +  1 '8888'
015900061026     O                       conta88mi     2   +  2
016000061026     OQSYSPRT   E            totali      1  1
016100061026     O                                              'Variazioni I0 per'
016200061026     O                                         +  1 '8888'
016300061026     O                       conta88i0     2   +  2
016400061026     O                                         +  5 'Variazioni I0 per'
016500061026     O                                         +  1 '8888 senza RSD2'
016600061026     O                       conta88i0parz 2   +  2
016700061026     OQSYSPRT   E            totali      1  1
016800061026     O                                         +  0 'Variazioni I0 per'
016900061026     O                                         +  1 '9999 consegnate'
017000061026     O                       conta99i0     2   +  2
017100061026     O                                         +  5 'Variazioni I0 per'
017200061026     O                                         +  1 '9999 consegnate '
017300061026     O                                         +  1 'senza RSD2 '
017400061026     O                       conta99i0parz 2   +  2
017500061026     OQSYSPRT   E            totali      1  1
017600061026     O                                              'Variazioni I0 per'
017700061026     O                                         +  1 '2bolla consegnate'
017800061026     O                       conta2bi0     2   +  2
017900061026     OQSYSPRT   E            bolla       1
018000061026     O                                           17 'Bolla con variaz.'
018100061026     O               03                        +  1 'MI x 8888'
018200061026     O               01                        +  1 'I0 x 8888'
018300061026     O               02                        +  1 'I0 x 9999'
018400061026     o                       blpaas           +   1
018500061026     o                       blplnp           +   1
018600061026     o                       blpnrs           +   1
018700061026     o                       blpnsp        2  +   1
018800061026     o                       blpcbo           +   1
018900061026     o                       blpksc           +   1
019000061026     o                       blpdcm           +   1 '    /  /  '
019100061026     o                       arbdtv           +   1 '    /  /  '
019200061026     O               04                        +  1 'non modificata rag.soc'
