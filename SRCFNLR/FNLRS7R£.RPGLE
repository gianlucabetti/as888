000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200060912      * FNLRS7R *----------------------------------------------------*
000300060912      *      Stampa lettere di vettura in arrivo con TELLUS    *
000400060912      *      (2° parte)
000500060912      *--------------------------------------------------------------*
000600060912     Fwfwlrs1l  UF   E           K DISK    usropn
000700060912     Fwfwlrs3l  UF   E           K DISK    usropn
000800060912     F                                     RENAME(wfWLRS00:wfWLRS03)
000900060216     FFiAR401L  UF   E           K DISK
001000000502     Fazorg01l  iF   E           K DISK
001100970219     D*
001200000427     D wris            S                   like(wlrris)
001300000427     D wtrc            S                   like(ar4trc)
001400000510     D wabl            S                   like(wlrabl)
001500030611     D wcontA4         S              3  0
001600030611     D wcontA5         S              3  0
001700000502     D prelnp          S                   like(db0lnp)
001800030611      *
001900000503     D comman          S             99
002000030611     D lung            S             15  5 inz(99)
002100030611     D w99             S              1    dim(99)
002200030611     D CMA4            S             99    dim(1) ctdata perrcd(1)
002300030611     D CMA5            S             99    dim(1) ctdata perrcd(1)
002400030611      *
002500970219     D* PARAMETRI RICEVUTI DAL PGM CHIAMANTE
002600000427     D fnlrs1ds      e DS
002700030624     D* DS ESTERNA PER - STAMPA BOLLE
002800030624     D fnlsb5ds      E DS
002900070215     d dDB0flo       e ds                  inz
003000000428     D dsbl4a        E DS
003100000502     D og143         E DS
003200940216     D KPJBA         E DS
003300970219     D CNCR80        E DS
003400970219     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003500040611
003600040611     d Status         sds
003700040611     d  SDSprm           *parms
003800040611
003900970219     C****************************************************************
004000970219     C*  RIEPILOGO INDICATORI
004100970219     C****************************************************************
004200970219     C* 01    - OFF ---> S T A M P A        ON ---> R I S T A M P A
004300970402     C* 30    - LETTURA
004400980102     C* 31/35 - COMODO
004500970219     C*****************************************************************
004600920422     C*
004700970219     C* 01 ON  - SI TRATTA DI RISTAMPA
004800000418    1C     ds1ris        IFEQ      'S'
004900920422     C                   SETON                                        01
005000970219    1C                   ENDIF
005100000427     c*
005200030611      * eseguo ovrprtf - A4
005300030611     C                   movea(P)  cma4(1)       w99
005400030611     C                   movea     ds1d90prb4    w99(24)
005500030611     C                   movea     ds1d90mdb4    w99(46)
005600030611     C                   movea     w99           comman
005700030611     C                   call      'QCMDEXC'
005800030611     C                   parm                    comman
005900030611     C                   parm                    lung
006000030611      * eseguo ovrprtf - A5
006100030611     C                   movea(P)  cma5(1)       w99
006200030611     C                   movea     ds1d90prb5    w99(24)
006300030611     C                   movea     ds1d90mdb5    w99(46)
006400030611     C                   movea     w99           comman
006500030611     C                   call      'QCMDEXC'
006600030611     C                   parm                    comman
006700030611     C                   parm                    lung
006800970219     C*****
006900970219     C* E L A B O R O
007000970219     C*****
007100970402     C*
007200030624     c                   z-add     0             wcontA4
007300030624     c                   z-add     0             wcontA5
007400000510     c*
007500000510     c* I° Giro: Elaboro e stampo le bolle abilitate
007600000427     c*
007700060912     c                   open      wfwlrs1l
007800000510     c                   z-add     0             wabl
007900060912     c     klrs1         setll     wfwlrs1l
008000060912     c     klrs1         reade     wfwlrs1l                               30
008100000427     c     *in30         doweq     *off
008200000510     c                   exsr      stampa
008300060912     c     klrs1         reade     wfwlrs1l                               30
008400000427     c                   enddo
008500060912     c                   close     wfwlrs1l
008600000510     c*
008700000510     c* I° Giro: Elaboro e stampo le bolle abilitate
008800000510     c*
008900060912     c                   open      wfwlrs3l
009000000510     c                   z-add     1             wabl
009100060912     c     klrs1         setll     wfwlrs3l
009200060912     c     klrs1         reade     wfwlrs3l                               30
009300000510     c     *in30         doweq     *off
009400000510     c                   exsr      stampa
009500060912     c     klrs1         reade     wfwlrs3l                               30
009600000510     c                   enddo
009700060912     c                   close     wfwlrs3l
009800911206      *
009900030624     c                   movel     'C'           DB0tla
010000030611     c                   call      DS1d90psl
010100030624     c                   parm                    fnlsb5ds
010200030624      *
010300030624     C                   EVAL        *inLR = *ON
010400970219     C*
010500000510     C*--- esegue operazioni per stampare le bolle -------------------*
010600000510     C     stampa        BEGSR
010700030624      *
010800030624     C                   CLEAR                   fnlsb5ds
010900070215     c                   clear                   dDB0flo
011000000510     C                   z-add     wlrAAS        DB0AAS
011100000510     C                   z-add     wlrLNP        DB0LNP
011200000510     C                   z-add     wlrNRS        DB0NRS
011300000510     C                   z-add     wlrNSP        DB0NSP
011400070215     ***C                   movel     wlrcgi        DB0flo
011500070215     c                   movel     wlrCGI        §DB0cgi
011600070215     c                   movel     wlrTGI        §DB0tgi
011700070215     c                   movel(p)  dDB0flo       DB0flo
011800000510     C                   MOVEL     'A'           DB0TBO
011900000510     C   01              MOVEL     'R'           DB0RIS
012000030624      *
012100000510     c* nei primi 2 bytes dei flag operativi devo passare che il chiamante
012200000510     c* e' la stampa l.v. in arrivo e se deve stampare "*" oppure no
012300030624     c                   move      'A'           DB0fl1
012400030624     c                   move      wlrast        DB0fl2
012500030624     c* se stampate 500 bolle, 250 x laser, imposto flag per chiudere prtf
012600030624     c     wcontA4       ifge      250
012700030624     c                   z-add     0             wcontA4
012800030624     c                   movel     '1'           DB0fl7
012900030624     c                   endif
013000030624     c     wcontA5       ifge      250
013100030624     c                   z-add     0             wcontA5
013200030624     c                   movel     '1'           DB0fl8
013300030624     c                   endif
013400000510     c* nel 5° byte dei flag operativi metto 'D' per stamparla in caso
013500000510     c* di bolla non abilitata
013600000510     c     wabl          ifeq      1
013700030624     c                   movel     'D'           DB0fl5
013800000510     c                   endif
013900030624      *
014000030624     c                   call      DS1d90psl
014100030624     c                   parm                    fnlsb5ds
014200030624      *
014300000510     C* AGGIORNO LA BOLLA
014400000510     c     *in01         ifeq      *off
014500040611     c     sdsprm        andle     1
014600060216     C     KAR4          CHAIN     FiAR401L                           31
014700000510     c                   movel     ar4not        dsbl4a
014800031029     c                   Select
014900031029     c                   When      §B4Abm = 'S'
015000031029     c                   Eval      §B4Abm = 'P'
015100031029     c                   When      §B4Abm = 'C'
015200031029     c                   Eval      §B4Abm = 'Q'
015300031113     c                   When      §B4Abm = 'K'
015400031113     c                   Eval      §B4Abm = 'W'
015500070420     c                   When      §B4Abm = 'N' or §b4abm  = 'Y'
015600000510     C                   MOVEL     'J'           §b4abm
015700031029     c                   EndSl
015800000510     c                   z-add     ds1dfa        §b4asl
015900000510     c                   movel     dsbl4a        ar4not
016000060216     C  N31              UPDATE    FiAR4000
016100000510     c                   endif
016200000510     c* deleto record da file di work
016300000510     c     wabl          ifeq      0
016400060912     c                   delete    wfwlrs00
016500000510     c                   else
016600060912     c                   delete    wfwlrs03
016700000510     c                   endif
016800000510     c
016900030624     c     DB0LNP        ifne      prelnp
017000030624     c                   z-add     DB0LNP        prelnp
017100000510     c                   clear                   og143
017200030624     c     DB0LNP        chain     azorg01l                           31
017300000510     c     *in31         ifeq      *off
017400000510     c                   movel     orgde3        og143
017500000510     c                   endif
017600000510     c                   endif
017700030611      *
017800030611      * Conto pagine per fnlsb0r
017900031024     c**!!!              IF        Ds1d90bsl = 'S'
018000030624     c                   Select
018100030624     c                   when      DB0TMS = '4'
018200030624     c                   add       1             wcontA4
018300030624     c                   when      DB0TMS = '5'
018400030624     c                   add       1             wcontA5
018500030624     c                   endsl
018600030611      *
018700000510     c                   endsr
018800970219     C*--- SR INIZIALE -----------------------------------------------*
018900970219     C     *INZSR        BEGSR
019000000426     C****  DATI PASSATI  ****
019100000426     C     *ENTRY        PLIST
019200000426     C                   PARM                    KPJBA
019300120111     c**                 Parm                    Fnlrt4ds
019400040611      * sono richiamato da stampa "ddt si" in arrivo
019500120111     c**                 if            SDSprm > 1
019600120111     c**                 Clear                   Fnlrs1ds
019700120111     c**                 Eval      ds1ris = dt4ris
019800120111     c**                 Eval      ds1lna = dt4lna
019900120111     c**                 Eval      ds1fgs = dt4lnp
020000120111     c*+                 Eval      ds1dfa = dt4dfa
020100120111     c**                 Eval      ds1d90prb4 = dt4d90prb4
020200120111     c**                 Eval      ds1d90prb5 = dt4d90prb5
020300120111     c**                 Eval      ds1d90mdb4 = dt4d90mdb4
020400120111     c**                 Eval      ds1d90mdb5 = dt4d90mdb5
020500120111     c**                 Eval      ds1d90psl = dt4d90psl
020600120111     c**                 Else
020700000426     C                   MOVEL     KPJBU         fnlrs1ds
020800120111     c**                 endif
020900000426     C****  REPERISCO SIMFEL  ****
021000000426     C                   Z-ADD     1             CODUT
021100000426     C                   CALL      'X§PARUT'
021200000426     C                   PARM                    UT§DSE
021300000426     C                   MOVEL     REC80         CNCR80
021400970219     C****
021500970219     C** KLIST
021600970219     C****
021700000427     C     Klrs1         KLIST
021800000419     C                   KFLD                    WRIS
021900000427     C                   KFLD                    ds1fgs
022000000427     C                   KFLD                    ds1lna
022100000510     C                   KFLD                    wabl
022200000427     C     KAR4          KLIST
022300000427     C                   KFLD                    wlraas
022400000427     C                   KFLD                    wlrlnp
022500000427     C                   KFLD                    wlrnrs
022600000427     C                   KFLD                    wlrnsp
022700000427     C                   KFLD                    WTRC
022800000427     C                   MOVEL     'A'           WTRC
022900000419     c* imposto flag ristampa per chain su file di work
023000040611     c**   ds1ris        ifeq      'S'
023100040611     c                   select
023200040611     c                   when      ds1ris = 'S'
023300000419     c                   movel     'R'           wris
023400040611     c                   when      ds1ris = 'V'
023500040611     c                   movel     'V'           wris
023600040611     c                   other
023700000419     c                   clear                   wris
023800040611     c**                 endif
023900040611     c                   endsl
024000970402     C*
024100970402     C                   ENDSR
024200970219     C*
024300030611**         CMA4
024400040407OVRPRTF FNLSB5PA4 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle§A4') SHARE(*YES) SAVE(*YES)
024500030611**         CMA5
024600040407OVRPRTF FNLSB5PA5 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle§A5') SHARE(*YES) SAVE(*YES)
