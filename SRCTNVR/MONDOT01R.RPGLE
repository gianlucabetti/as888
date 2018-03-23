000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300100112     Ftabel00f  if   e           K DISK
000400100112     FWFMON00F  uF   E           K DISK
000500100112     FFNblp31L  iF   E           K DISK
000600091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700091125
000800100112     D* RICHIAMO PGM DI TASSAZIONE BOLLE - TNSF20R
000900100112     D PARAMT          DS
001000100112     D  PARCA                256    256
001100100112     D DTAS          E DS
001200100112     D DTASV         E DS
001300100112     D  TSV                    1     20
001400100112     D                                     DIM(20)
001500100112     D  TVA                   21    140P 3
001600100112     D                                     DIM(20)
001700100112     D  TEV                  141    160
001800100112     D                                     DIM(20)
001900100112     D DS3A          E DS
002000100112     D kpjba         E DS
002100100112     d totpes          s             12  0
002200100112     d totvol          s             13  0
002300100112     d w0140           s             14  0
002400100112     d alfarmn         s             14
002500100112     d kcod            s                   like(tblcod)
002600100112     d kkey            s                   like(tblkey)
002700021214      *--------------------------------------------------
002800100112     c     *entry        plist
002900100112     c                   parm                    kpjba
003000100112
003100100112     C                   Z-ADD     1             CODUT             1 0
003200100112
003300100113     C     Kmondo        KLIST
003400100113     C                   KFLD                    kfile             1
003500100113     C                   KFLD                    alfarmn
003600100112     C     KTAB1         KLIST
003700100112     C                   KFLD                    CODUT
003800100112     C                   KFLD                    KCOD
003900100112     C                   KFLD                    KKEY
004000070913     c     kblp          klist
004100091125     c                   kfld                    lnp               3 0
004200091125     c                   kfld                    aas               4 0
004300091125     c                   kfld                    mgs               4 0
004400100112     c                   z-add     049           lnp
004500100111     c                   z-add     2010          aas
004600100113     c                   z-add     0113          mgs
004700091125
004800070913     c
004900091125     c     kblp          setll     fnblp31l
005000091125     c     kblp          reade     fnblp31l
005100051110
005200091125    1c                   DOW       not %EOF(FNblp31l)
005300100111     c*
005400100112     c                   if        blpksc=0493666
005500100112     c
005600100112     C* PREPARO LA DS PER IL PGM DI TASSAZIONE
005700100112     C                   CLEAR                   DTAS
005800100112     C                   CLEAR                   PARCA
005900100112     C*
006000100112     C                   CLEAR                   DTASV
006100100112     C                   CLEAR                   KPJBU
006200100112     C* CODICE BOLLA
006300100112     C     SAVCBO        IFNE      BLPCBO
006400100112     C                   MOVEL     '3A'          KCOD
006500100112     C                   MOVEL     *BLANKS       KKEY
006600100112     C                   MOVEL     BLPCBO        KKEY
006700100112     C     KTAB1         CHAIN     TABEL00F                           30
006800100112     C   30              MOVEL     'F1'          §3ATB1
006900100112     C  N30              MOVEL     TBLUNI        DS3A
007000100112     C                   MOVEL     §3ATB1        FLGTB1            1
007100100112     C**
007200100112     C                   MOVEL     BLPCBO        SAVCBO
007300100112     C     *LIKE         DEFINE    BLPCBO        SAVCBO
007400100112     C                   ENDIF
007500100112     C*
007600100112     C* IMPOSTO UN COD.TASSAZIONE INESISTENTE NEL COD.TASSAZ.MITTENTE
007700100112     C*   PRECHE' IN PARTENZA NON VENGONO CONTROLLATI GLI ASSEGNATI
007800100112     C                   MOVEL     'Z9'          TASMCT
007900100112     C*
008000100112     C* SE E' FRANCO SEMPRE CODICE CLIENTE+LINEA PARTENZA
008100100112     C                   MOVEL     BLPKSC        TASKSC
008200100112     C                   Z-ADD     BLPPKF        TASPKF
008300100112     C                   Z-ADD     BLPVLF        TASVLF
008400100112     C                   Z-ADD     BLPNCL        TASNCL
008500100112     C                   MOVEL     BLPCTS        TASCTS
008600100121     c* Per spedizioni fino a 100 kg --> uso tariffa 100
008700100121     c*                oltre         --> uso tariffa 001
008800100121     c                   if        blppkf<=100
008900100121     C                   z-add     100           TASCTR
009000100121     c                   else
009100100121     C                   z-add     001           TASCTR
009200100121     c                   endif
009300100112     C                   Z-ADD     BLPMGS        TASDSP
009400100112     C                   MOVEL     BLPAAS        TASDSP
009500100112     C                   MOVEL     TASDSP        TASDCT
009600100112     C                   MOVEL     BLPCAD        TASCAD
009700100112     C                   MOVEL     BLPNZD        TASNZD
009800100112     C                   MOVEL     BLPFIN        TASFIN
009900100112     C                   MOVEL     BLPCAM        TASCAM
010000100112     C                   MOVEL     BLPNZM        TASNZM
010100100112     C                   MOVEL     BLPFAP        TASFAP
010200100112     C                   MOVEL     BLPTSP        TASTSP
010300100112     C                   MOVEL     BLPTC1        TASTC1
010400100112     C                   MOVEL     BLPTC2        TASTC2
010500100112     C                   MOVEL     BLPVAS        TASVAS
010600100112     C                   MOVEL     BLPFDN        TASFDN
010700100112     C                   Z-ADD     BLPLNP        TASLNP
010800100112     C                   Z-ADD     BLPLNA        TASLNA
010900100112     C                   MOVEL     §3ATB1        TASTBL
011000100112     C                   Z-ADD     BLPIAS        TASIAS
011100100112     C                   Z-ADD     BLPQFT        TASQFT
011200100112     C* FLAG SI NO DDT
011300100112     C     BLPFST        IFEQ      'S'
011400100112     C     BLPFST        OREQ      'C'
011500100112     C                   MOVEL     'S'           TASDDT
011600100112     C                   ENDIF
011700100112     C                   MOVEL     PARAMT        KPJBU
011800100112     C*
011900100112     C                   CALL      'TNSF20R'                            99
012000100112     C                   PARM                    KPJBA
012100100112     C                   PARM                    DTAS
012200100112     C                   PARM                    DTASV
012300100112     c**
012400100112     c                   exsr      Fileout
012500091125     c                   endif
012600091125     c
012700091125     c     kblp          reade     fnblp31l
012800091125     c                   enddo
012900021216      *
013000021214     C                   eval       *inlr = *on
013100100112     c*.-------------------------------------------------------------
013200100112     c     FileOUT       BEGSR
013300100112     c                   eval      w0140=blprmn
013400100112     c                   movel     w0140         alfarmn
013500100113     c* Vedo di quale file si tratta
013600100113     c                   select
013700100113     c                   when      blprma='SEP2009XXX '
013800100113     c                   eval      kfile='A'
013900100113     c                   when      blprma='SEP20091ST '
014000100113     c                   eval      kfile='B'
014100100113     c                   when      blprma='SEP20092ND '
014200100113     c                   eval      kfile='C'
014300100113     c                   when      blprma='SEP20093RD '
014400100113     c                   eval      kfile='D'
014500100113     c                   ENDSL
014600100112     c
014700100113     c     kmondo        chain     wfmon00f
014800100112    1c                   if        not %found(wfmon00f)
014900100112     c                   except    errore
015000100112   x1c                   else
015100100121     c                   clear                   mon_ctr
015200100121     c                   clear                   mon_imv
015300100112     c                   clear                   mon_por
015400100112     c                   clear                   mon_sv0
015500100112     c                   clear                   mon_sv1
015600100112     c                   clear                   mon_sv2
015700100112     c                   clear                   mon_sv3
015800100112     c                   clear                   mon_sv4
015900100112     c                   clear                   mon_sv5
016000100112     c                   clear                   mon_sv6
016100100112     c                   clear                   mon_sv7
016200100112     c                   clear                   mon_sv8
016300100112     c                   clear                   mon_sv9
016400100112     c                   clear                   mon_va0
016500100112     c                   clear                   mon_va1
016600100112     c                   clear                   mon_va2
016700100112     c                   clear                   mon_va3
016800100112     c                   clear                   mon_va4
016900100112     c                   clear                   mon_va5
017000100112     c                   clear                   mon_va6
017100100112     c                   clear                   mon_va7
017200100112     c                   clear                   mon_va8
017300100112     c                   clear                   mon_va9
017400100112
017500100121     c                   eval      mon_ctr=tasctr
017600100121     c
017700100113     c                   eval      mon_pkf=blppkf
017800100113     c                   eval      mon_vlf=blpvlf
017900100112
018000100112     c                   eval      mon_imv=tasimv
018100100112     c                   eval      mon_por=taspor
018200100112
018300100112    2c                   do        10            xx                3 0
018400100112    3c                   if        tsv(1)<>' '
018500100112     c                   eval      mon_sv0=tsv(1)
018600100112     c                   eval      mon_va0=tva(1)
018700100112     c                   endif
018800100112     c                   if        tsv(2)<>' '
018900100112     c                   eval      mon_sv1=tsv(2)
019000100112     c                   eval      mon_va1=tva(2)
019100100112     c                   endif
019200100112     c                   if        tsv(3)<>' '
019300100112     c                   eval      mon_sv2=tsv(3)
019400100112     c                   eval      mon_va2=tva(3)
019500100112     c                   endif
019600100112     c                   if        tsv(4)<>' '
019700100112     c                   eval      mon_sv3=tsv(4)
019800100112     c                   eval      mon_va3=tva(4)
019900100112     c                   endif
020000100112     c                   if        tsv(5)<>' '
020100100112     c                   eval      mon_sv4=tsv(5)
020200100112     c                   eval      mon_va4=tva(5)
020300100112     c                   endif
020400100112     c                   if        tsv(6)<>' '
020500100112     c                   eval      mon_sv5=tsv(6)
020600100112     c                   eval      mon_va5=tva(6)
020700100112     c                   endif
020800100112     c                   if        tsv(7)<>' '
020900100112     c                   eval      mon_sv6=tsv(7)
021000100112     c                   eval      mon_va6=tva(7)
021100100112     c                   endif
021200100112     c                   if        tsv(8)<>' '
021300100112     c                   eval      mon_sv7=tsv(8)
021400100112     c                   eval      mon_va7=tva(8)
021500100112     c                   endif
021600100112     c                   if        tsv(9)<>' '
021700100112     c                   eval      mon_sv8=tsv(9)
021800100112     c                   eval      mon_va8=tva(9)
021900100112     c                   endif
022000100112     c                   if        tsv(10)<>' '
022100100112     c                   eval      mon_sv9=tsv(10)
022200100112     c                   eval      mon_va9=tva(10)
022300100112     c                   endif
022400100112     c                   enddo
022500100112
022600100112     c                   update    wfmond00
022700100112     c                   endif
022800100112
022900100112     c                   ENDSR
023000091125     OQSYSPRT   E            errore      1
023100100112     O                                              'Non trovato RMN'
023200100112     O                       blprmn           +   1
023300100112     O                                        +   2 'Cad'
023400100112     O                       blpCAD           +   1
023500100112     O                                        +   2 'Lod'
023600100112     O                       blplod           +   1
023700100112     O                                        +   2 'Prd'
023800100112     O                       blpprd           +   1
