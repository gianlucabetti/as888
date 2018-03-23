000100130312     /*PRM dbgview(*source)
000200130312     /*END
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400060912      * FNLRS7R *----------------------------------------------------*
000500060912      *      Stampa lettere di vettura in arrivo con TELLUS    *
000600060912      *      (2° parte)
000700060912      *--------------------------------------------------------------*
000800060912     Fwfwlrs1l  UF   E           K DISK    usropn
000900060912     Fwfwlrs3l  UF   E           K DISK    usropn
001000060912     F                                     RENAME(wfWLRS00:wfWLRS03)
001100060216     FFiAR401L  UF   E           K DISK
001200000502     Fazorg01l  iF   E           K DISK
001300970219     D*
001400000427     D wris            S                   like(wlrris)
001500000427     D wtrc            S                   like(ar4trc)
001600000510     D wabl            S                   like(wlrabl)
001700030611     D wcontA4         S              3  0
001800030611     D wcontA5         S              3  0
001900000502     D prelnp          S                   like(db0lnp)
002000030611      *
002100000503     D comman          S             99
002200030611     D lung            S             15  5 inz(99)
002300030611     D w99             S              1    dim(99)
002400030611     D CMA4            S             99    dim(1) ctdata perrcd(1)
002500030611     D CMA5            S             99    dim(1) ctdata perrcd(1)
002600030611      *
002700970219     D* PARAMETRI RICEVUTI DAL PGM CHIAMANTE
002800000427     D fnlrs1ds      e DS
002900030624     D* DS ESTERNA PER - STAMPA BOLLE
003000030624     D fnlsb5ds      E DS
003100130312     d FNLSB6ds1     e ds                  inz
003200130312     d   DB6pdf      e                     inz('F')
003300070215     d dDB0flo       e ds                  inz
003400000428     D dsbl4a        E DS
003500000502     D og143         E DS
003600940216     D KPJBA         E DS
003700970219     D CNCR80        E DS
003800970219     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003900040611
004000040611     d Status         sds
004100040611     d  SDSprm           *parms
004200040611
004300970219     C****************************************************************
004400970219     C*  RIEPILOGO INDICATORI
004500970219     C****************************************************************
004600970219     C* 01    - OFF ---> S T A M P A        ON ---> R I S T A M P A
004700970402     C* 30    - LETTURA
004800980102     C* 31/35 - COMODO
004900970219     C*****************************************************************
005000920422     C*
005100970219     C* 01 ON  - SI TRATTA DI RISTAMPA
005200000418    1C     ds1ris        IFEQ      'S'
005300920422     C                   SETON                                        01
005400970219    1C                   ENDIF
005500000427     c*
005600030611      * eseguo ovrprtf - A4
005700030611     C                   movea(P)  cma4(1)       w99
005800030611     C                   movea     ds1d90prb4    w99(24)
005900030611     C                   movea     ds1d90mdb4    w99(46)
006000030611     C                   movea     w99           comman
006100030611     C                   call      'QCMDEXC'
006200030611     C                   parm                    comman
006300030611     C                   parm                    lung
006400030611      * eseguo ovrprtf - A5
006500030611     C                   movea(P)  cma5(1)       w99
006600030611     C                   movea     ds1d90prb5    w99(24)
006700030611     C                   movea     ds1d90mdb5    w99(46)
006800030611     C                   movea     w99           comman
006900030611     C                   call      'QCMDEXC'
007000030611     C                   parm                    comman
007100030611     C                   parm                    lung
007200970219     C*****
007300970219     C* E L A B O R O
007400970219     C*****
007500970402     C*
007600030624     c                   z-add     0             wcontA4
007700030624     c                   z-add     0             wcontA5
007800000510     c*
007900000510     c* I° Giro: Elaboro e stampo le bolle abilitate
008000000427     c*
008100060912     c                   open      wfwlrs1l
008200000510     c                   z-add     0             wabl
008300060912     c     klrs1         setll     wfwlrs1l
008400060912     c     klrs1         reade     wfwlrs1l                               30
008500000427     c     *in30         doweq     *off
008600000510     c                   exsr      stampa
008700060912     c     klrs1         reade     wfwlrs1l                               30
008800000427     c                   enddo
008900060912     c                   close     wfwlrs1l
009000000510     c*
009100000510     c* I° Giro: Elaboro e stampo le bolle abilitate
009200000510     c*
009300060912     c                   open      wfwlrs3l
009400000510     c                   z-add     1             wabl
009500060912     c     klrs1         setll     wfwlrs3l
009600060912     c     klrs1         reade     wfwlrs3l                               30
009700000510     c     *in30         doweq     *off
009800000510     c                   exsr      stampa
009900060912     c     klrs1         reade     wfwlrs3l                               30
010000000510     c                   enddo
010100060912     c                   close     wfwlrs3l
010200130312      *
010300130312      * Stampa eventuali documenti PDF abbinati alle spediz. stampate
010400130320if  1c                   if        DB6num > *zero
010500130312     c                   clear                   FNLSB5ds
010600130314     c                   eval      DB0fa4 = 'I'
010700130314     c                   eval      DB6pdf = 'S'
010800130312     c                   call      DS1d90psl
010900130312     c                   parm                    FNLSB5ds
011000130312     c                   parm                    FNLSB6ds1
011100130312e   1c                   endif
011200911206      *
011300130312      * Fine
011400030624     c                   movel     'C'           DB0tla
011500030611     c                   call      DS1d90psl
011600030624     c                   parm                    fnlsb5ds
011700030624      *
011800030624     C                   EVAL        *inLR = *ON
011900970219     C*
012000000510     C*--- esegue operazioni per stampare le bolle -------------------*
012100000510     C     stampa        BEGSR
012200030624      *
012300030624     C                   CLEAR                   fnlsb5ds
012400070215     c                   clear                   dDB0flo
012500000510     C                   z-add     wlrAAS        DB0AAS
012600000510     C                   z-add     wlrLNP        DB0LNP
012700000510     C                   z-add     wlrNRS        DB0NRS
012800000510     C                   z-add     wlrNSP        DB0NSP
012900070215     ***C                   movel     wlrcgi        DB0flo
013000070215     c                   movel     wlrCGI        §DB0cgi
013100070215     c                   movel     wlrTGI        §DB0tgi
013200070215     c                   movel(p)  dDB0flo       DB0flo
013300000510     C                   MOVEL     'A'           DB0TBO
013400000510     C   01              MOVEL     'R'           DB0RIS
013500030624      *
013600000510     c* nei primi 2 bytes dei flag operativi devo passare che il chiamante
013700000510     c* e' la stampa l.v. in arrivo e se deve stampare "*" oppure no
013800030624     c                   move      'A'           DB0fl1
013900030624     c                   move      wlrast        DB0fl2
014000030624     c* se stampate 500 bolle, 250 x laser, imposto flag per chiudere prtf
014100030624     c     wcontA4       ifge      250
014200030624     c                   z-add     0             wcontA4
014300030624     c                   movel     '1'           DB0fl7
014400030624     c                   endif
014500030624     c     wcontA5       ifge      250
014600030624     c                   z-add     0             wcontA5
014700030624     c                   movel     '1'           DB0fl8
014800030624     c                   endif
014900000510     c* nel 5° byte dei flag operativi metto 'D' per stamparla in caso
015000000510     c* di bolla non abilitata
015100000510     c     wabl          ifeq      1
015200030624     c                   movel     'D'           DB0fl5
015300000510     c                   endif
015400030624      *
015500030624     c                   call      DS1d90psl
015600030624     c                   parm                    fnlsb5ds
015700130312     c                   parm                    FNLSB6ds1
015800030624      *
015900000510     C* AGGIORNO LA BOLLA
016000000510     c     *in01         ifeq      *off
016100040611     c     sdsprm        andle     1
016200060216     C     KAR4          CHAIN     FiAR401L                           31
016300000510     c                   movel     ar4not        dsbl4a
016400031029     c                   Select
016500031029     c                   When      §B4Abm = 'S'
016600031029     c                   Eval      §B4Abm = 'P'
016700031029     c                   When      §B4Abm = 'C'
016800031029     c                   Eval      §B4Abm = 'Q'
016900031113     c                   When      §B4Abm = 'K'
017000031113     c                   Eval      §B4Abm = 'W'
017100070420     c                   When      §B4Abm = 'N' or §b4abm  = 'Y'
017200000510     C                   MOVEL     'J'           §b4abm
017300031029     c                   EndSl
017400000510     c                   z-add     ds1dfa        §b4asl
017500000510     c                   movel     dsbl4a        ar4not
017600060216     C  N31              UPDATE    FiAR4000
017700000510     c                   endif
017800000510     c* deleto record da file di work
017900000510     c     wabl          ifeq      0
018000060912     c                   delete    wfwlrs00
018100000510     c                   else
018200060912     c                   delete    wfwlrs03
018300000510     c                   endif
018400000510     c
018500030624     c     DB0LNP        ifne      prelnp
018600030624     c                   z-add     DB0LNP        prelnp
018700000510     c                   clear                   og143
018800030624     c     DB0LNP        chain     azorg01l                           31
018900000510     c     *in31         ifeq      *off
019000000510     c                   movel     orgde3        og143
019100000510     c                   endif
019200000510     c                   endif
019300030611      *
019400030611      * Conto pagine per fnlsb0r
019500031024     c**!!!              IF        Ds1d90bsl = 'S'
019600030624     c                   Select
019700130325     c                   when      DB0tms = '5'  or  DB0tms = 'I'
019800130325     c*//                add       1             wcontA5
019900130325     c                   add       1             wcontA4
020000130325     c*//                when      DB0tms = '4'  or  DB0tms = 'J'
020100130325     c                   other
020200130325     c                   add       1             wcontA4
020300030624     c                   endsl
020400030611      *
020500000510     c                   endsr
020600970219     C*--- SR INIZIALE -----------------------------------------------*
020700970219     C     *INZSR        BEGSR
020800000426     C****  DATI PASSATI  ****
020900000426     C     *ENTRY        PLIST
021000000426     C                   PARM                    KPJBA
021100120111     c**                 Parm                    Fnlrt4ds
021200040611      * sono richiamato da stampa "ddt si" in arrivo
021300120111     c**                 if            SDSprm > 1
021400120111     c**                 Clear                   Fnlrs1ds
021500120111     c**                 Eval      ds1ris = dt4ris
021600120111     c**                 Eval      ds1lna = dt4lna
021700120111     c**                 Eval      ds1fgs = dt4lnp
021800120111     c*+                 Eval      ds1dfa = dt4dfa
021900120111     c**                 Eval      ds1d90prb4 = dt4d90prb4
022000120111     c**                 Eval      ds1d90prb5 = dt4d90prb5
022100120111     c**                 Eval      ds1d90mdb4 = dt4d90mdb4
022200120111     c**                 Eval      ds1d90mdb5 = dt4d90mdb5
022300120111     c**                 Eval      ds1d90psl = dt4d90psl
022400120111     c**                 Else
022500000426     C                   MOVEL     KPJBU         fnlrs1ds
022600120111     c**                 endif
022700000426     C****  REPERISCO SIMFEL  ****
022800000426     C                   Z-ADD     1             CODUT
022900000426     C                   CALL      'X§PARUT'
023000000426     C                   PARM                    UT§DSE
023100000426     C                   MOVEL     REC80         CNCR80
023200970219     C****
023300970219     C** KLIST
023400970219     C****
023500000427     C     Klrs1         KLIST
023600000419     C                   KFLD                    WRIS
023700000427     C                   KFLD                    ds1fgs
023800000427     C                   KFLD                    ds1lna
023900000510     C                   KFLD                    wabl
024000000427     C     KAR4          KLIST
024100000427     C                   KFLD                    wlraas
024200000427     C                   KFLD                    wlrlnp
024300000427     C                   KFLD                    wlrnrs
024400000427     C                   KFLD                    wlrnsp
024500000427     C                   KFLD                    WTRC
024600000427     C                   MOVEL     'A'           WTRC
024700000419     c* imposto flag ristampa per chain su file di work
024800040611     c**   ds1ris        ifeq      'S'
024900040611     c                   select
025000040611     c                   when      ds1ris = 'S'
025100000419     c                   movel     'R'           wris
025200040611     c                   when      ds1ris = 'V'
025300040611     c                   movel     'V'           wris
025400040611     c                   other
025500000419     c                   clear                   wris
025600040611     c**                 endif
025700040611     c                   endsl
025800970402     C*
025900970402     C                   ENDSR
026000970219     C*
026100030611**         CMA4
026200040407OVRPRTF FNLSB5PA4 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle§A4') SHARE(*YES) SAVE(*YES)
026300030611**         CMA5
026400040407OVRPRTF FNLSB5PA5 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle§A5') SHARE(*YES) SAVE(*YES)
