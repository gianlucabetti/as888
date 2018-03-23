000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060209     H* UNIBO03R *---------------------------------------------------*
000300060209     H*         - UNISCO FNBL4 FNAR4 IN FIAR400F
000400000000     H*--------------------------------------------------------------*
000500060209     fFNar401L  if   e           k Disk
000600060209     f                                     prefix(bl4:3)
000700060209     fFNBL401L  if   e           k Disk
000800051114     fFnarb01L  if   e           k Disk
000900051114     fFnBLp01L  if   e           k Disk
001000060217     ftabel00f  if   e           k Disk
001100051012     F
001200051012     F* FILE NUOVO
001300060209     fFIAR401L  uf a e           k Disk    usropn
001400051012     F
001500051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001600051012     D
001700060223     d Te              s             66    Dim(17) Ctdata Perrcd(1)
001800060209     d ERR             s             35    Dim(5) Ctdata Perrcd(1)
001900051019     d OVRP            s             78    Dim(1) Ctdata Perrcd(1)
002000060217     d c7k             s              1    Dim(50)
002100060217     d c7ktra          s              1    Dim(50)
002200060217     d c7ktmc          s              1    Dim(50)
002300060215     d
002400060217     D ds7k          E DS
002500060217     D dsbl4a        E DS
002600060215     D dsbl4ab4      E DS                  extname(dsbl4a)
002700060215     d                                     prefix(b4)
002800051012     D
002900051012     d STAFILE         s              5
003000060209     d werrore         s              1
003100060213     d savnot          s                   like(bl4not)
003200060213     d savftr          s                   like(bl4ftr)
003300060213     d savdtr          s                   like(bl4dtr)
003400060209     D Crige           C                   CONST('BOLLA RIGENERATA DALLA SEDE')
003500051012     C
003600060209     c     Kbolla        Klist
003700060209     c                   Kfld                    bl4aas
003800060209     c                   Kfld                    bl4lnp
003900060209     c                   Kfld                    bl4nrs
004000060209     c                   Kfld                    bl4nsp
004100060209     c     Kbl4          Klist
004200060209     c                   Kfld                    bl4aas
004300060209     c                   Kfld                    bl4lnp
004400060209     c                   Kfld                    bl4nrs
004500060209     c                   Kfld                    bl4nsp
004600060209     c                   Kfld                    bl4trc
004700060217     C     KTAB1         KLIST
004800060217     C                   KFLD                    KKUT
004900060217     C                   KFLD                    KCOD              2
005000060217      *
005100060217      *  Effettuo caricamento dei tipi record da trasmettere
005200060217     c
005300060217     C                   Z-ADD     0             X                 3 0
005400060217     C                   Z-ADD     0             y                 3 0
005500060217     C                   Z-ADD     0             z                 3 0
005600060217     C                   Z-ADD     1             KKUT              1 0
005700060217     C                   MOVEL     '7K'          KCOD
005800060217     c
005900060217     C     KTAB1         CHAIN     TABEL                              30
006000060217     c
006100060217     C     *IN30         DOWEQ     '0'
006200060217     C     X             ANDLT     100
006300060217     C     TBLFLG        IFEQ      *BLANKS
006400060217     C                   MOVEL     TBLUNI        DS7K
006500060217     c
006600060217     C     §7KTRA        ifeq      *BLANKS
006700060217     C     §7KTmc        andeq     *BLANKS
006800060217     C                   ADD       1             X
006900060217     C                   MOVEL     TBLKEY        C7K(X)
007000060217     C                   END
007100060217     c
007200060217     c                   if        §7ktmc<>*blanks
007300060217     C                   ADD       1             y
007400060217     C                   MOVEL     TBLKEY        C7Ktmc(y)
007500060217     C                   END
007600060217     C                   END
007700060217     c
007800060217     C     KTAB1         READE     TABEL                                  30
007900060217     C                   ENDdo
008000060217      *
008100051012     c***
008200060209     c                   open      fiar401l
008300051019     c* Eseguo ovrprtf su qsysprt
008400051019     C                   MOVEA     ovrp          COMMAN           80
008500051019     C                   CALL      'QCMDEXC'                            47
008600051019     C                   PARM                    COMMAN
008700051019     C                   PARM      80            LUNG             15 5
008800051019     c
008900051019     c                   open      qsysprt
009000051019     c
009100051012     c***
009200051012     c                   except    TESTA
009300980130     C***
009400060209     C* FNBL400F
009500980130     C***
009600060209     C     *LOVAL        SETLL     FNBL401L
009700060209     C                   READ      FNBL401L
009800051012     C
009900060209    1C                   DOW       NOT %EOF(FNBL401L)
010000051012     C                   SETOFF                                       0102
010100051012     c
010200060209     c* Se record già presente in fiar4
010300060209     c     kbl4          chain     fiar401l
010400060209    2c                   if        %found(fiar401l)
010500051012     c* conto
010600051013     c                   seton                                        04
010700051013     c                   except    errore
010800051013     c                   setoff                                       04
010900060209     c                   add       1             giabl4            9 0
011000051012   X2c                   else
011100051012     c
011200051114     C* VERIFICO SE c'e' record in fnblp
011300060209     c     kbolla        chain     fnblp01l
011400051114    3c                   if        not %found(fnblp01l)
011500051114     c* Stampo errore manca bolla
011600051012     c* non lo stampo
011700060209     c                   eval      stafile='FNBL4'
011800051012     c                   seton                                        01
011900051012     c                   except    Errore
012000051012     c                   setoff                                       01
012100051114     c                   add       1             noblp             7 0
012200051114   x3c                   else
012300051012     c
012400060209     c                   eval      ar4aas=bl4aas
012500060209     c                   eval      ar4lnp=bl4lnp
012600060209     c                   eval      ar4nrs=bl4nrs
012700060209     c                   eval      ar4nsp=bl4nsp
012800060209     c                   eval      ar4trc=bl4trc
012900060209     c                   eval      ar4not=bl4not
013000060209     c                   eval      ar4ftr=bl4ftr
013100060209     c                   eval      ar4dtr=bl4dtr
013200060209     c                   clear                   ar4duv
013300060209     c                   clear                   ar4flo
013400060217     c
013500060217     c* Se bolla non trasmessa flaggo tutto
013600060217     c                   if        blpft2=' '
013700060217     c                   eval      ar4ftr='T'
013800060217     c                   eval      ar4dtr=blpdim
013900060217     c                   eval      ar4duv=blpdim
014000060217     c                   endif
014100060217     c
014200060217     c* Se bolla sflaggata ma da non inviare a clienti, la flaggo
014300060217     c                   if        ar4ftr=' ' or ar4dtr=0
014400060217     c     ar4trc        lookup    c7ktmc                                 35
014500060223     c                   if        not *in35
014600060217     c                   eval      ar4ftr='T'
014700060217     c                   eval      ar4dtr=77777777
014800060217     c                   endif
014900060217     c                   endif
015000051012     c
015100060209     c                   write     fiar4000
015200060209     c                   add       1             aggbl4            9 0
015300051012    3c                   endif
015400051012    2c                   endif
015500051012     C
015600060209     C                   READ      FNBL401L
015700051012    1c                   enddo
015800051012     C
015900051012     c***
016000060209     C*   FNAR400f
016100051012     C***
016200060209     C     *LOVAL        SETLL     FNar401L
016300060209     C                   READ      FNar401L
016400051012     C
016500060209    1C                   DOW       NOT %EOF(FNar401L)
016600051012     C                   SETOFF                                       0302
016700051012     c
016800060209     c* Se record già presente in fiar4, non lo scrivo
016900060213     c     kbl4          chain     fiar401l
017000060209     c
017100060213   1ac                   if        %found(fiar401l)
017200060213     c* Scarto il record "W"  e Z tengo buoni quelli della partenza
017300060213   1bc                   if        bl4trc='Z'  or bl4trc='W'
017400060213     c                   add       1             recZW             9 0
017500060213   1bc                   else
017600060221     c
017700060221     c* Se sflaggato vince sempre l'arrivo, senza guardare  ar4not<>
017800060223   1cc                   if        (bl4ftr=' ' and bl4dtr<>99999999) or
017900060223     c                             bl4dtr=0
018000060221     c                   eval      ar4not=bl4not
018100060221     c                   eval      ar4ftr=bl4ftr
018200060221     c                   eval      ar4dtr=bl4dtr
018300060221     c                   update    fiar4000
018400060223     c                   add       1             aggsflagg         9 0
018500060223   1cc                   else
018600060213     c
018700060209     c                   eval      werrore='S'
018800060209     c* se tipo record A updato ar4not
018900060209    2c                   select
019000060209    2c                   when      ar4trc='A'
019100060215     c* dalla partenza tengo solo i numeri stop
019200060215     c                   movel     ar4not        dsbl4a
019300060215     c* dalla arrivo gli altri campi
019400060215     c                   movel     bl4not        dsbl4ab4
019500060215     c
019600060215     c                   eval      b4§b4asr=§b4asr
019700060215     c                   eval      b4§b4asc=§b4asc
019800060215     c                   eval      ar4not=dsbl4ab4
019900060209     c                   update    fiar4000
020000060209     c                   add       1             aggTRCA           9 0
020100060209     c                   eval      werrore='N'
020200060209     c
020300060209     c* Verifico se il campo nota è =
020400060209     c* se diverso stampo errore
020500060209    2c                   when      bl4not<>ar4not
020600060214     c                   setoff                                       20
020700060209     c
020800060209    3c                   select
020900060213     c
021000060213     c* se tipo record D updato ar4not  ma stampo quello che c'era in bl4
021100060213    2c                   when      ar4trc='D'
021200060217     c                   eval      savnot=ar4not
021300060217     c                   eval      savftr=ar4ftr
021400060217     c                   eval      savdtr=ar4dtr
021500060213     c                   eval      ar4not=bl4not
021600060213     c                   update    fiar4000
021700060213     c                   add       1             aggTRCD           9 0
021800060214     c                   seton                                        20
021900060213     c
022000060209     c* vince quello di fnbl4
022100060209    3c                   when      ar4trc='R' or ar4trc='S' or ar4trc='T'
022200060209     c                             or ar4trc='U' or ar4trc='V'
022300060209     c                   add       1             noaggTRCl         9 0
022400060209     c                   eval      werrore='N'
022500060209     c
022600060209     c* Se tipo record 8 o 9 , escludo dall'errore e dall'aggiornamento
022700060209     c*  se la nota è "BOLLA RIGENETATA DALLA SEDE"
022800060209    3c                   when      ar4trc='8' or ar4trc='9'
022900060209    4c                   if        bl4not=Crige
023000060209     c* non faccio nulla e non do errore
023100060209     c                   add       1             noaggrige         9 0
023200060209     c                   eval      werrore='N'
023300060209   x4c                   else
023400060209    5c                   if        ar4not=Crige
023500060209     c* updato e non stampo errore
023600060209     c                   eval      ar4not=bl4not
023700060209     c                   update    fiar4000
023800060209     c                   add       1             aggrige           9 0
023900060209     c                   eval      werrore='N'
024000060214     c
024100060214     c* se nessuna delle due è nota di "bolla rigenerata" tengo l'arrivo
024200060214     c                   else
024300060217     c                   eval      savnot=ar4not
024400060217     c                   eval      savftr=ar4ftr
024500060217     c                   eval      savdtr=ar4dtr
024600060214     c                   eval      ar4not=bl4not
024700060214     c                   update    fiar4000
024800060214     c                   add       1             aggTRC89          9 0
024900060214     c                   seton                                        20
025000060209    5c                   endif
025100060209    4c                   endif
025200060209    3c                   endsl
025300060209     c
025400060209    3c                   if        werrore='S'
025500060209     c                   eval      stafile='FNAR4'
025600060214     c* Se tipo record D o 8 o  9  stampo i dati da bl4
025700060214    4c                   if        *in20
025800060213     c                   eval      bl4not=savnot
025900060213     c                   eval      bl4ftr=savftr
026000060213     c                   eval      bl4dtr=savdtr
026100060213     c                   eval      stafile='FNBL4'
026200060213    4c                   endif
026300060209     c                   seton                                        05
026400060209     c                   except    Errore
026500060209     c                   setoff                                       05
026600060214     c
026700060214    4c                   if        not *in20
026800060209     c                   add       1             giaar4div         9 0
026900060213    4c                   endif
027000060213    3c                   endif
027100060209     c
027200060209   x2c                   other
027300060209     c* altrimenti conto senza stampare
027400060209     c                   add       1             giaar4            9 0
027500060209    2c                   endsl
027600060221     c
027700060221   1cc                   endif
027800060221   1bc                   endif
027900060221     c
028000060209   1ac                   else
028100051012     c
028200051114     C* VERIFICO SE c'e' record in fnARb
028300060209     c     kbolla        chain     fnarb01l
028400051114    3c                   if        not %found(fnARb01l)
028500051114     c* Stampo errore manca bolla
028600060209     c                   eval      stafile='FNAR4'
028700051012     c                   seton                                        03
028800051012     c                   except    Errore
028900051012     c                   setoff                                       03
029000051114     c                   add       1             noarb             7 0
029100051012     c
029200051012   X3c                   else
029300051114     c
029400060209     c                   eval      ar4aas=bl4aas
029500060209     c                   eval      ar4lnp=bl4lnp
029600060209     c                   eval      ar4nrs=bl4nrs
029700060209     c                   eval      ar4nsp=bl4nsp
029800060209     c                   eval      ar4trc=bl4trc
029900060209     c                   eval      ar4not=bl4not
030000060209     c                   eval      ar4ftr=bl4ftr
030100060209     c                   eval      ar4dtr=bl4dtr
030200060209     c                   clear                   ar4duv
030300060209     c                   clear                   ar4flo
030400051012     c
030500060209     c                   write     fiar4000
030600060209     c                   add       1             aggar4            9 0
030700051012    3c                   endif
030800051012     c
030900060213   1ac                   endif
031000051012     C
031100060209     C                   READ      FNar401L
031200051012    1c                   enddo
031300051012     c***
031400060209     C     GIABL4        ADD       GIAAR4        GIATOT            9 0
031500060209     C     AGGBL4        ADD       AGGAR4        AGGTOT            9 0
031600051012     C
031700051012     c                   except    totali
031800051012     c***
031900020502     C                   SETON                                        LR
032000051012     c*------------------------------------------------------------------
032100051012
032200051012      *  T E S T A T A
032300051012     oQsysprt   e            Testa            02
032400051012     o                       Te(1)               66
032500051012     o                       Te(2)              132
032600051114     o                       *Date              120 '  /  /    '
032700051012     o                       Page          Z    132
032800051012     O
032900051012     o          e            ERRORE      1
033000051012     o                       STAFILE
033100060209     o                       BL4aas           +   1
033200060209     o                       BL4LNp           +   1
033300060209     o                       BL4NRS           +   1
033400060209     o                       BL4NSp        2  +   1
033500060209     o                       BL4trc           +   1
033600060209     o                       BL4not           +   1
033700060213     o                       BL4ftr           +   1
033800060213     o                       BL4dtr           +   1
033900051012     o               01      err(1)             132
034000051012     o               03      err(3)             132
034100051013     o               04      err(4)             132
034200060209     o               05      err(5)             132
034300051012     o
034400051012     o          e            TOTALI      2
034500051012     o                       Te(3)               66
034600051012     o                       Te(4)              132
034700060209     o                       GIABL4        2     38
034800051114     o                       NOBLp         2     67
034900060209     o                       AGGBL4        2    132
035000051012     O
035100051012     o          e            TOTALI      2
035200051012     o                       Te(5)               66
035300051012     o                       Te(6)              132
035400060209     o                       GIAAR4        2     38
035500051114     o                       NOARb         2     67
035600060209     o                       AGGTRCA       2    110
035700060209     o                       AGGAR4        2    132
035800060209     o          e            TOTALI      2
035900060209     o                       Te(8)               66
036000060209     o                       Te(9)              132
036100060209     o                       giaar4div     2     67
036200060209     o                       noAGGTRCl     2    110
036300060213     o          e            TOTALI      2
036400060213     o                       Te(14)              66
036500060213     o                       reczw         2     67
036600060213     o          e            TOTALI      2
036700060213     o                       Te(15)              66
036800060213     o                       aggtrcd       2     67
036900060214     o          e            TOTALI      2
037000060214     o                       Te(16)              66
037100060214     o                       aggtrc89      2     67
037200060223     o          e            TOTALI      2
037300060223     o                       Te(17)              66
037400060223     o                       aggsflagg     2     67
037500060209     o          e            TOTALI      2
037600060209     o                       Te(10)              66
037700060209     o                       Te(11)             132
037800060209     o                       aggrige       2    110
037900060209     o          e            TOTALI      2
038000060209     o                       Te(12)              66
038100060209     o                       Te(13)             132
038200060209     o                       noaggrige     2    110
038300051012     O
038400051012     o          e            TOTALI      2
038500051012     o                       Te(7)               66
038600051013     o                       GIATOT        2     43
038700051012     o                       AGGTOT        2     67
038800051012**         TE
038900060209                            **  UNISCI FNBL400F FNAR400F IN   F I    1
039000060209A R 4 O O F  **             UNIBO03R                     PAG. XXXX   2
039100060209FNBL400F : già in Fiar400f xxx.xxx.xxx  Non presente FNBLPx.xxx.xx   3
039200051114x                                            Aggiornatixxx.xxx.xxx   4
039300060209FNAR400F : già in Fiar400f xxx.xxx.xxx  Non presente FNARBx.xxx.xx   5
039400060209x    TipRec "A"     riaggiornati xxx.xxx.xxx Aggiornatixxx.xxx.xxx   6
039500060209TOTALI GENERALI:già in Fiar400f xxx.xxx.xxx  AGGIORNATI XXX.XXX.XXX  7
039600060209           già in Fiar400f con campo AR4NOT diverso       x.xxx.xx   8
039700060209x    TipRec "RSTUV" no aggiornatixxx.xxx.xxx                         9
039800060209           già in Fiar400f con nota "BOLLA RIGENERATA DA SEDE" ria   10
039900060209ggiornata: tengo la reale di ar4 xxx.xxx.xxx                         11
040000060209           già in Fiar400f con nota diversa da "BOLLA RIGENERATA D   12
040100060209A SEDE"che non riaggiorno da ar4 xxx.xxx.xxx                         13
040200060213           già in Fiar400f tengo da BL4 rec "Z"-"W"       x.xxx.xx   14
040300060213           già in Fiar400f tengo da AR4 rec "D"           x.xxx.xx   15
040400060214           già in Fiar400f tengo da AR4 rec "8"-"9"       x.xxx.xx   16
040500060223           già in Fiar400f tengo da AR4 sflaggati         x.xxx.xx   17
040600051012**    ERR
040700051114MANCA bolla in FNBLP01L                                              1
040800051114
040900051114MANCA bolla in FNARB01L
041000060209Già presente in FIAR4
041100060209Già presente in FIAR4 ma <> NOT
041200051019**
041300060209OVRPRTF FILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES) USRDTA('FIAR400F')
