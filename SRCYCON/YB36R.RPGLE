000200020812      /TITLE RICONCILIAZIONE MANUALE MOVIMENTI BANCA
000300020809     H DFTACTGRP(*NO) ACTGRP('QILE')
000400020809     H BNDDIR('PJXBND') BNDDIR('PJCBND')
000500000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000600020812
000700990830     Fndmov01l  IF   E           K DISK
000800990824     Fndbmv03l  IF   E           K DISK
000900910213     F                                     INFDS(BMVDS)
001000990827     Fndbmv00f  UF   E             DISK
001100990824     F                                     RENAME(ndbmv000:ndbmv)
001200990827     F                                     commit
001300990827     F                                     usropn
001400990824     Fndbmb03l  IF   E           K DISK
001500910213     F                                     INFDS(BMBDS)
001600990824     Fndbmb00f  UF   E             DISK
001700990824     F                                     RENAME(ndbmb000:ndbmb)
001800990827     F                                     commit
001900990827     F                                     usropn
002000990824     Fndbsr00f  O  A E           K DISK
002100990827     F                                     commit
002200990827     F                                     usropn
002300990824     FYB36D     CF   E             WORKSTN
002400000000     F                                     SFILE(DSF1:NRR1)
002500000000     F                                     SFILE(DSF2:NRR2)
002600000000     F                                     INFDS(INFO)
002700020812
002800020809     D SKI             S              2    DIM(26) CTDATA PERRCD(26)
002900990902     D SKB             S             15  2 DIM(26)                              MOV. BANCA
003000990902     D SKM             S             15  2 DIM(26)                              MOV. CONTAB.
003100990902     D dva             S              6  0 DIM(26)
003200990903     D nmr             S              5  0 DIM(26)                              N. movimenti blocco
003300990902     D letds           ds
003400020809     D LET                            2    DIM(26)                              LETTERA sf2
003500990902     D BPR             S              9  0 DIM(26)                              NUMERO RIC.
003600990902     D DOP             S              6  0 DIM(26)                              DATA OPERAZ
003700990902MM   D FLG             S              6  0 DIM(26)                              FLAG REGIST
003800000000     D KPJBA         E DS
003900021128     D KPJBUs          S                   like(Kpjbu)
004000990827     D ANA399DS      E DS                  inz
004100021128     D YB361Ds       E DS                  inz
004200990824     D xsoc001ds     e ds                  inz
004300990824     D    XscTesRid                   1    overlay(XscSkp:1)                    Scadenz. retroattivo
004400000000     D                 DS
004500000000     D  SPE                   10     10
004600000000     D  BMVOPZ                 1     10
004700910213     D BMBDS           DS
004800910213     D  NRRBMB               397    400B 0
004900910213     D BMVDS           DS
005000910213     D  NRRBMV               397    400B 0
005100000000     D INFO            DS
005200000000     D  RIG                  370    370
005300000000     D                 DS
005400000000     D  CURSOR                 1      2B 0
005500000000     D  R$$                    2      2
005600020809     D WRKWDW251       S              1
005700020813     D A               S              5I 0
005800020905     D I               S              5I 0
005900020813     D K               S              5I 0
006000021025     D SavRcdNbr       S
006100021025     D                                     LIKE(RecNbr)
006200031002     D WrkSelBMV       S              1
006300031002     D                                     INZ('0')
006400031002     D WrkSelBMB       S              1
006500031002     D                                     INZ('0')
006600990824
006700020809     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006800020809    >D CONTESDS      E DS
006900020809     D ndc250ds      e ds                  inz
007000020809     D ndc251ds      e ds                  inz
007100020809     D Ndc252Ds      E DS                  Inz
007200990824     D ndct21ds      e ds                  inz
007300020812     D NdCT41DS      e ds                  inz
007400990824     D nd016ds       e ds                  inz
007500990824     D nd017ds       e ds                  inz
007600990824     D ana088ds      e ds                  inz
007700990824     D Operazione      S              2
007800990824     D Banca           S                   Like(rapv)
007900990824     D Linea           S                   Like(lineav)
008000990824     D StrutturaO      S             10
008100990824     D LenOut          S              9B 0
008200990824     D Esito           S              1
008300990825     D OutPut          S           4000
008400990824     D NoBCO           S              1
008500990825     D wslv            S                   like(bmvimporto)
008600990827     D d1dopiiso       s               d
008700990827     D d1dopfiso       s               d
008800990830     D dtsaldofiso     s               d
008900990830     D wrknaschr       s             10
009000990830     D wrkdescr        s                   like(bmbdescr)
009100990901     D nrdocchr        s              6
009200990903     D xnmdat          s               d
009300990903     D xnmdag          s               d
009400990824
009500000111     D sndpm01         s              7                                         Message id
009600000111     D sndpm02         s             20                                         Message file name
009700000111     D sndpm03         s            256                                         Immediate text
009800000111     D sndpm04         s              9b 0 inz(%size(sndpm03))                  Length of text
009900000111     D sndpm05         s             10    inz('*INFO')                         Message type
010000000111     D sndpm06         s             16    inz('*EXT')                          Call stack entry
010100000111     D sndpm07         s              9b 0                                      Call stack counter
010200000111     D sndpm08         s              4                                         Call stack counter
010300000111     D/COPY QSYSINC/QRPGLESRC,QUSEC
010400031002
010500031002     D LngTx01         S            128                                         Text string
010600031002     D LngTx02         S             10I 0                                      Length text string
010700031002     D                                     INZ(%SIZE(LngTx01))
010800031002     D LngTx03         S              7                                         Message ID
010900031002     D                                     INZ('BAR0019')
011000031002     D LngTx04         S             20                                         Qualified MSGF
011100031002     D                                     INZ('YBARMSG   *LIBL')
011200000111
011300920312     C*-----
011400920312?    C     INIZIO        TAG
011500920312     C*
011600920312     C                   MOVEL     *BLANK        RAPV
011700990824     C                   clear                   lineav
011800990827     C                   MOVEL     'M'           TMAN              1
011900990827AM   C                   MOVEL     'S'           TSFL              1
012000920312     C*
012100000000     C*-- 1° VIDEATA --------------------------------------*
012200920312     C*
012300920312?    C     VIDEO         TAG
012400920312     C*
012500920303AM   C                   SETOFF                                       78
012600990827     C* Decodifico in presenza di errori.                  *
012700990827     C                   if        *in50
012800990827     C                   setoff                                       50
012900990827     C                   write     CB36D1
013000990827     C                   seton                                        50
013100990827     C                   endif
013200990827     C*                                                    *
013300000000     C                   EXFMT     CB36D1                                       *
013400900420     C   KC              GOTO      FINE                                         CMD12
013500990827     C                   SETOFF                                       1550      *
013600000000     C                   SETOFF                                       22  40    *
013700000000     C*                                                    *
013800000000     C*---ESEGUE I CONTROLLI DATI VIDEO                    *
013900990824
014000990824     C*---------------------------------------------------------------------
014100990824     C                   if        *inkd
014200990824    >C                   if        h1nmfl = 'RAPV'
014300990824    >C                             or h1nmfl = 'LINEAV'
014400990824     C                   RESET                   ana088ds
014500990824     C                   MOVEL     rapv          banca088
014600990824     C                   MOVEL     lineav        linea088
014700990824     C                   MOVEL     '01'          OPZ088
014800990824     C                   MOVEL     ana088ds      KPJBU
014900990824     C                   CALL      'ANA088R'
015000990824     C                   PARM                    KPJBA
015100990824     C                   MOVEL     KPJBU         ana088DS
015200990824     C     banca088      IFNE      *BLANK
015300990824     C                   MOVEL     Banca088      rapv
015400990824     C                   MOVEL     Linea088      lineav
015500990824     C                   ENDIF
015600990824     C                   endif
015700990830     C                   seton                                        50
015800990824     C                   endif
015900990824
016000990824     C* Controllo rapporto bancario.
016100990830     C                   clear                   d1desban
016200990830     C                   clear                   d1divisa
016300990824     C                   Eval      LenOut = %Size(ND016DS)
016400990824     C                   callb     'NDDVABRA'
016500990824     C                   PARM      '01'          Operazione
016600990824     C                   PARM                    xscsoc
016700990824     C                   PARM      rapv          Banca
016800990824     C                   PARM      lineav        Linea
016900990824     C                   PARM                    *omit
017000990824     C                   PARM                    *omit
017100990824     C                   PARM      'ND016DS'     StrutturaO
017200990824     C                   PARM                    LenOut
017300990824     C                   PARM      '0'           Esito
017400990824     C                   PARM                    nd016ds
017500990824     C                   if        esito = '1'
017600990827     C                   seton                                        1550
017700990830     C                   else
017800990830     C     de2ban016     cat       de2lin016:1   d1desban
017900990830     C                   eval      d1divisa = divisa016
018000990824     C                   endif
018100990902     C* F15=Interrogazioni.
018200990902     C                   if        *inkp and not *in15
018300990902     C                   exsr      callana399
018400990902     C                   seton                                        50
018500990902     C                   endif
018600990830     C* Controllo data saldo finale.
018700990830     C                   eval      dtsaldofiso = *hival
018800990830     C                   if        d1dtsaldof > 0
018900990830     C     *dmy          test(de)                d1dtsaldof
019000990830     C                   if        %error
019100990903     C                   seton                                        1450
019200990830     C                   else
019300990830     C     *dmy          move      d1dtsaldof    dtsaldofiso
019400990830     C                   endif
019500990830     C                   endif
019600990830     C* Reperisco data ultima riconciliazione e saldo.
019700990830     C                   EXSR      SALDO
019800990827     C* Controllo data operazione iniziale.
019900990827     C                   eval      d1dopiiso = *loval
020000990827     C                   if        d1dopi > 0
020100990827     C     *dmy          test(de)                d1dopi
020200990827     C                   if        %error
020300990827     C                   seton                                        1250
020400990827     C                   else
020500990827     C     *dmy          move      d1dopi        d1dopiiso
020600990827     C                   endif
020700990827     C                   endif
020800990827     C* Controllo data operazione finale.
020900990827     C                   eval      d1dopfiso = *hival
021000990827     C                   if        d1dopf > 0
021100990827     C     *dmy          test(de)                d1dopf
021200990827     C                   if        %error
021300990827     C                   seton                                        1350
021400990827     C                   else
021500990827     C     *dmy          move      d1dopf        d1dopfiso
021600990827     C                   endif
021700990827     C                   endif
021800990903     C* Controllo date in sequenza.
021900990903     C                   if        d1dopiiso > d1dopfiso
022000990903     C                   seton                                        121350
022100990903     C                   endif
022200990827     C*
022300990827     C   50              goto      video
022400990830     C   kb              goto      video
022500000000     C*
022600990830
022700000000     C*...ENTER  O.K. PROSEGUO !                           *
022800000000     C*
022900000000     C*...RIEMPIO I CAMPI DELLA TESTATA....
023000000000     C*                                                    *
023100990827     C     TMAN          COMP      'A'                                    05
023200000000     C*
023300000000     C*   05 -  MANUTENZIONE AUTOMATICA
023400000000     C*  N05 -  MANUTENZIONE MANUALE.
023500000000     C*
023600000000     C*...Descrizione banca.
023700990824     C     de2ban016     cat(p)    de2lin016:1   desban
023800990824
023900000000     C*
024000000000     C*...CARICAMENTO DEL SUBFILE.
024100000000     C*                                                    *
024200000000     C*...Inizializzo il sub-file.                         *
024300000000     C                   Z-ADD     1             RECNBR                         *
024400000000     C                   Z-ADD     1             PAGINA                         *
024500000000     C                   Z-ADD     1             RECNB2                         *
024600000000     C                   Z-ADD     1             PAGIN2                         *
024700920312     C                   Z-ADD     0             NRR2              5 0
024800000000     C                   Z-ADD     0             NUMRE2
024900920312     C                   Z-ADD     0             NRR1              5 0
025000000000     C                   Z-ADD     0             NUMREC
025100920312AM   C                   Z-ADD     0             NRR3              5 0
025200000000     C                   SETON                                        60        *
025300000000     C                   SETOFF                                       61        *
025400000000     C                   SETOFF                                       62        *
025500000000     C                   WRITE     DCT1                                         *
025600000000     C                   WRITE     DCT2                                         *
025700000000     C                   SETON                                        61        *
025800000000     C                   SETON                                        62        *
025900000000     C                   SETOFF                                       6070      *
026000990902     C                   clear                   letds
026100000000     C*                                                    *
026200000000     C*...Carico il sub-file della BANCA.                  *
026300000000     C*
026400920302AM   C     TSFL          IFEQ      'S'
026500990825     C     k03bmb03      SETLL     ndbmb000                               90
026600000000?    C     R15           TAG                                                                 SF
026700000000     C*
026800990825     C     k03bmb03      READE     ndbmb000                               90
026900000000     C*
027000990902     C                   if        not *in90
027100990902     C                   if        bmbstatus <> *blanks
027200990902     C                             or
027300990902     C                             bmbdtope < d1dopiiso
027400990902     C                             or
027500990902     C                             bmbdtope > d1dopfiso
027600990915     C                             or
027700990915     C                             bmbann = 1
027800000000     C                   GOTO      R15
027900000000     C                   ELSE
028000920312     C                   Z-ADD     0             NRR1
028100000000     C                   Z-ADD     0             NUMREC
028200000000?    C                   EXSR      CARSB1                                       *
028300000000     C                   END
028400990902     C                   endif
028500990902
028600000000     C*                                                    *
028700000000     C*...Carico il sub-file della CONTABILITA'.           *
028800000000     C*
028900990825     C     KEY           SETLL     ndbmv000
029000000000?    C     R172          TAG                                                    *
029100000000     C*
029200990825     C     KEY           READE     ndbmv000                               99
029300990830     C* Scarto i movimenti non definitivi.
029400990830     C  N99bmvtpregz     cabne     'D'           R172
029500990830     C* Scarto i movimenti già riconciliati.
029600990902     C                   if        not *in99
029700990902     C                   if        bmvstatus <> *blanks
029800990902     C                             or
029900990902     C                             bmvdtope < d1dopiiso
030000990902     C                             or
030100990902     C                             bmvdtope > d1dopfiso
030200000000     C                   GOTO      R172
030300000000     C                   ELSE
030400920312     C                   Z-ADD     0             NRR2
030500000000     C                   Z-ADD     0             NUMRE2
030600000000?    C                   EXSR      CARSB2                                       *
030700000000     C                   END
030800990902     C                   endif
030900000000     C*                                                    *
031000000000     C     PIENO         IFEQ      *BLANK
031100000000     C     PIEN1         ANDEQ     *BLANK
031200000000     C                   SETON                                        40
031300000000     C                   GOTO      VIDEO
031400000000     C                   END
031500000000     C*                                                    *
031600000000     C*--- EMISSIONE SUBFILE                               *
031700000000     C*                                                    *
031800000000?    C     DISP1         TAG                                                    *
031900990902     C*
032000990902     C                   eval      c2separa = *all'-'
032100990902     C                   if        letds <> *blanks
032200990902     C                   eval      c2separa = 'Usate: '
032300020812     C                             + %trim(letds) + ' '
032400020812     C                             + c2separa
032500990902     C                   endif
032600990902     C*
032700910522     C                   SETOFF                                       6465
032800000000     C     NRR1          IFEQ      0
032900000000     C                   SETOFF                                           61    *
033000000000     C                   END
033100000000     C     NRR2          IFEQ      0
033200000000     C                   SETOFF                                           62    *
033300000000     C                   END
033400021025
033500021025     ** In CT1CSRRRN c'è il numero di riga del subfile
033600021025     ** su cui era il cursore quando l'utente ha premuto un tasto.
033700021025     ** Impostando RecNbr   ,
033800021025     ** visualizzo la stessa pagina che vedeva l'utente
033900021025     ** quando ha premuto l'ultimo tasto.
034000021025     C                   IF        CT1CsrRrn > *ZERO
034100021025     C                   EVAL      RecNbr    = CT1CsrRrn
034200021025     C                   ELSE
034300021025     C                   EVAL      RecNbr    = SavRcdNbr
034400021025     C                   ENDIF
034500021025     ** Se non so quale pagina visualizzare, forzo pagina 1.
034600021025     C                   IF        RecNbr    < 1
034700021025     C                             OR
034800021025     C                             RecNbr    > NRR1
034900021025     C                   EVAL      RecNbr    = 1
035000021025     C                   ENDIF
035100021025     ** Salvo il record number del subfile.
035200021025     C                   EVAL      SavRcdNbr = RecNbr
035300020812
035400000000     C                   WRITE     CB36D2                                       *
035500020813     C                   WRITE     DCT1                                         *
035600000000     C                   EXFMT     DCT2                                         *
035700910522     C                   READ      DCT1                                   63    *
035800000000     C*                                                    *
035900900420     C   KC              GOTO      FINE                                         *
036000990827     C* F15=Interrogazioni.                                *
036100990827     C                   if        *inkp
036200990827     C                   exsr      callana399
036300990827     C                   goto      disp1
036400990827     C                   endif
036500020812
036600020812     ** Spengo gli indicatori di errore.
036700020812     C                   EVAL      *IN33 = *OFF
036800020812     C                   EVAL      *IN39 = *OFF
036900020812     C                   EVAL      *IN42 = *OFF
037000000000     C                   SETOFF                                       50        *
037100020812
037200910212     C                   MOVE      *ZERO         CURSOR                         *
037300910212     C                   MOVE      RIG           R$$                            *
037400910212     C                   SETOFF                                       55        *
037500020812     C   56CURSOR        COMP      15                                 565556    *
037600000000     C*                                                    *
037700000000     C*                                                    *
037800900420     C   KL              GOTO      VIDEO                                        *
037900000000     C*                                                    *
038000000000     C*...richiesto rollup nelle banche.
038100000000     C   55              DO
038200000000?    C  N90              EXSR      CARSB1                                       *
038300910213     C     NRR1          DIV       7             RECNBR
038400910213     C                   MVR                     W050              5 0
038500910522VMG  C*--
038600910522     C                   Z-ADD     RECNBR        PAGINA
038700910522     C     W050          IFNE      0
038800910522     C                   ADD       1             PAGINA
038900910522     C                   END
039000910522VMG  C*--
039100910213     C                   MULT      7             RECNBR
039200910213     C     W050          IFEQ      0
039300910212     C     RECNBR        SUB       6             RECNBR                 1111
039400910212     C   11              Z-ADD     1             RECNBR                         *
039500910213     C                   ELSE
039600910213     C                   ADD       1             RECNBR
039700910213     C                   END
039800000000     C                   SETON                                            61    *
039900000000     C                   SETOFF                                           62    *
040000021025     C                   EVAL      CT1CsrRrn = RecNbr
040100000000     C                   GOTO      DISP1
040200000000     C                   END
040300000000     C*                                                    *
040400000000     C*...richiesto rollup nella contabilita'.             *
040500000000     C   56              DO
040600000000?    C  N99              EXSR      CARSB2                                       *
040700020812     C     NRR2          DIV       7             RECNB2
040800910213     C                   MVR                     W050              5 0
040900910522VMG  C*--
041000910522     C                   Z-ADD     RECNB2        PAGIN2
041100910522     C     W050          IFNE      0
041200910522     C                   ADD       1             PAGIN2
041300910522     C                   END
041400910522VMG  C*--
041500020812     C                   MULT      7             RECNB2
041600910213     C     W050          IFEQ      0
041700020812     C     RECNB2        SUB       6             RECNB2                 1111
041800910213     C   11              Z-ADD     1             RECNB2                         *
041900910213     C                   ELSE
042000910213     C                   ADD       1             RECNB2
042100910213     C                   END
042200000000     C                   SETON                                            62    *
042300000000     C                   SETOFF                                           61    *
042400000000     C                   GOTO      DISP1
042500000000     C                   END
042600000000     C*                                                    *
042700000000     C*                                                    *
042800000000     C*...Controllo i dati modificati e determino i saldi.
042900000000?    C                   EXSR      CONTA                                        *
043000000000     C*                                                    *
043100000000     C*...Per ENTER o per ERRORE torno ad emettere il sfl.
043200000000     C*   Se e' errore ma e' stata richiesta la ric. manuale
043300000000     C*   non emetto l'errore ma lo lascio proseguire.
043400000000     C*
043500000000     C  N50              DO                                                     *
043600910522     C   64              DO
043700000000     C*...Imposto pagina richiesta nel sfl-1.              *
043800000000     C     PAGINA        MULT      7             RECNBR                         *
043900000000     C     RECNBR        SUB       6             RECNBR                 1111
044000000000     C   11              Z-ADD     1             RECNBR                         *
044100910522VMG  C     RECNBR        IFGT      NUMREC                                       *
044200000000     C                   Z-ADD     1             RECNBR                         *
044300000000     C                   Z-ADD     1             PAGINA                         *
044400000000     C                   END                                                    *
044500910522     C                   SETON                                            61    *
044600910522     C                   SETOFF                                           62    *
044700910522     C                   END                                                    *
044800000000     C*                                                    *
044900910522     C   65              DO
045000000000     C*...Imposto pagina richiesta nel sfl-2.              *
045100020812     C     PAGIN2        MULT      7             RECNB2                   11    *
045200020812     C     RECNB2        SUB       6             RECNB2                 1111
045300000000     C   11              Z-ADD     1             RECNB2                         *
045400910522VMG  C     RECNB2        IFGT      NUMRE2                                       *
045500000000     C                   Z-ADD     1             RECNB2                         *
045600000000     C                   Z-ADD     1             PAGIN2                         *
045700000000     C                   END                                                    *
045800910522     C                   SETON                                            62    *
045900910522     C                   SETOFF                                           61    *
046000910522     C                   END                                                    *
046100000000     C                   END
046200000000     C   50
046300000111     CANN05              GOTO      DISP1                                        *
046400000000     C*                                                    *
046500000119     C                   if        *inkf
046600000000?    C                   EXSR      AGGIOR                                       *
046700000111     C                   else
046800000111     C                   goto      disp1
046900000111     C                   endif
047000000000     C*                                                    *
047100000000     C                   GOTO      VIDEO                                        *
047200920302AM   C******************************************************
047300920303AM   C                   ELSE
047400920302AM   C*                                                    *
047500920302AM   C*...Carico il sub-file della CONTABILITA'.           *
047600920302AM   C*
047700990825AM   C     KEY           SETLL     ndbmv000
047800920302AM   C     R173          TAG                                                    *
047900920302AM   C*
048000990825AM   C     KEY           READE     ndbmv000                               99
048100990830AM   C  N99bmvtpregz     cabne     'D'           R173
048200920302AM   C*
048300990902     C                   if        not *in99
048400990902     C                             and
048500990902     C                             (bmvstatus <> *blanks
048600990902     C                             or
048700990902     C                             bmvdtope < d1dopiiso
048800990902     C                             or
048900990902     C                             bmvdtope > d1dopfiso)
049000920302AM   C                   GOTO      R173
049100920302AM   C                   ELSE
049200920302AM   C                   END
049300920302AM   C*                                                    *
049400920302AM   C*--- EMISSIONE SUBFILE                               *
049500920302AM   C*                                                    *
049600920302AM   C     DISP3         TAG                                                    *
049700920302AM   C                   SETOFF                                       65
049800920302AM   C     NRR3          IFEQ      0
049900920302AM   C                   SETOFF                                           62    *
050000920302AM   C                   END
050100920302AM   C                   WRITE     CB36D2                                       *
050200920302AM   C*                                                    *
050300920302AM   C   KC              GOTO      FINE                                         *
050400920302AM   C*                                                    *
050500920302AM   C                   SETOFF                                       50        *
050600920302AM   C*                                                    *
050700920302AM   C   KL              GOTO      VIDEO                                        *
050800920302AM   C*                                                    *
050900920302AM   C*...richiesto rollup nella contabilita'.             *
051000920302AM   C   56              DO
051100920302AM   C*--
051200920302AM   C     W050          IFNE      0
051300920302AM   C                   END
051400920302AM   C*--
051500920302AM   C     W050          IFEQ      0
051600920302AM   C                   ELSE
051700920302AM   C                   END
051800920302AM   C                   SETON                                            62    *
051900920302AM   C                   GOTO      DISP3
052000920302AM   C                   END
052100920302AM   C*                                                    *
052200920302AM   C*                                                    *
052300920302AM   C*                                                    *
052400920302AM   C*...Per ENTER o per ERRORE torno ad emettere il sfl.
052500920302AM   C*   Se e' errore ma e' stata richiesta la ric. manuale
052600920302AM   C*   non emetto l'errore ma lo lascio proseguire.
052700920302AM   C*
052800920302AM   C  N50              DO                                                     *
052900920302AM   C   65              DO
053000920302AM   C                   SETON                                            62    *
053100920302AM   C                   END                                                    *
053200920302AM   C                   END
053300920302     C   50
053400920302AM   CANN05
053500920302AM   CORNKF              GOTO      DISP3                                        *
053600920302AM   C*                                                    *
053700920312AM   C                   GOTO      VIDEO
053800920312AM   C                   END
053900920312     C*-----------
054000920312     C     FINE          TAG
054100920312     C                   SETON                                        LR
054200920312      /EJECT
054300910220     C*---------------------------------------------------------------*---
054400910220     C*  RIPRESA SALDI
054500910220     C*---------------------------------------------------------------*---
054600910220     C     SALDO         BEGSR
054700990830     C                   clear                   d1saldoi
054800990830     C                   clear                   d1giaric
054900990830     C                   clear                   d1daric
055000990824
055100990830     C* Reperisco data ultima riconciliazione.
055200990830     C                   clear                   ndct21ds
055300990830     C                   eval      societat21 = xscsoc
055400990830     C                   eval      ctbt21 = 'CG'
055500990830     C                   eval      bancat21 = rapv
055600990830     C                   eval      lineat21 = lineav
055700990830     C                   eval      divisat21 = divisa016
055800990830     C                   eval      solochit21 = '1'
055900990830     C                   call      'NDCT21R'
056000990830     C                   parm                    ndct21ds
056100990903     C                   eval      wslv = slfidopt21 - slfiaopt21
056200990830     C                   if        dtulchit21 > *loval
056300990830     C     *dmy          move      dtulchit21    d1dur
056400990830     C                   endif
056500990830     C* Reperisco saldi.
056600990830     C                   clear                   ndct21ds
056700990830     C                   eval      societat21 = xscsoc
056800990830     C                   eval      ctbt21 = 'CG'
056900990830     C                   eval      bancat21 = rapv
057000990830     C                   eval      lineat21 = lineav
057100990830     C                   eval      divisat21 = divisa016
057200990830     C                   eval      defint21 = '1'
057300990830     C                   eval      provvt21 = '0'
057400990830     C                   eval      gestit21 = '0'
057500990830     C                   eval      ripresat21 = '1'
057600990830     C                   eval      solochit21 = '0'
057700990830     C                   if        d1dur > 0
057800990830     C     *dmy          move      d1dur         datadat21
057900990830     C                   endif
058000990830     C                   adddur    1:*d          datadat21
058100990830     C                   call      'NDCT21R'
058200990830     C                   parm                    ndct21ds
058300990830     C* Saldo iniziale.
058400990830     C                   eval      d1saldoi = slindopt21 - slinaopt21
058500990824     C**********************************************************************
058600990824
058700990830     C* Calcolo totale movimenti già riconciliati.
058800990830     C                   if        d1saldof <> 0
058900990830     C                   eval      bmvdtope = datadat21
059000990830     C     k05bmv03      SETLL     ndbmv03l
059100990830     C     k04bmv03      READE     ndbmv03l
059200990830     C                   dow       not %eof
059300990830     C                             and bmvdtope <= dtsaldofiso
059400910220     C*  totalizzo i proposti
059500990825     C     bmvstatus     IFEQ      '1'
059600990830     C     bmvtpregz     andeq     'D'
059700990825     C     BMVDAre       IFEQ      1
059800990825     C                   ADD       bmvimporto    WSLV
059900910220     C                   ELSE
060000990825     C                   SUB       bmvimporto    WSLV
060100910220     C                   END
060200910220     C                   END
060300990830     C     k04bmv03      READE     ndbmv03l
060400990830     C                   ENDdo
060500990830     C                   eval      d1giaric = wslv
060600990830     C* Calcolo importo da riconciliare.
060700990830     C                   eval      d1daric = d1saldof - d1saldoi - d1giaric
060800990830     C                   endif
060900990830
061000910220     C                   ENDSR
061100000000     C/EJECT
061200000000     C*---------------------------------------------------------------*---
061300000000     C*  CONTROLLO LE MODIFICHE EFFETTUATE SE SONO O.K. COME IMPORTO  *
061400000000     C*---------------------------------------------------------------*---
061500000000     C     CONTA         BEGSR
061600000000     C*
061700000000     C                   Z-ADD     0             SKB
061800000000     C                   Z-ADD     0             SKM
061900990827     C                   clear                   dva
062000000000     C                   Z-ADD     0             SQUA
062100000000     C                   MOVE      *BLANK        LET
062200020812     C                   SETOFF                                       31
062300000000     C                   SETOFF                                       38
062400031002     C                   RESET                   WrkSelBMB
062500031002     C                   RESET                   WrkSelBMV
062600000000     C*
062700000000     C*...Calcola tot. sfl-2.
062800000000     C*
062900000000     C     NRR2          CABEQ     0             CON100
063000000000     C*
063100000000     C                   READC     DSF2                                   21
063200000000     C   21              SETOFF                                           62
063300000000     C*
063400000000     C     *IN21         DOWEQ     '0'
063500000000     C*
063600000000 IF2 C     RICSN         IFNE      *BLANK
063700020813     C                   Z-ADD     1             A
063800020809     C                   EVAL      RicSN = %TRIML(RicSN)
063900000000     C     RICSN         LOOKUP    SKI(A)                                 10
064000910213     C     LET(A)        IFEQ      *BLANK
064100020809     C                   MOVEL     RICSN         LET(A)
064200000000     C                   END
064300000000     C                   ADD       IMPORT        SKM(A)
064400031002     C                   EVAL      WrkSelBmv = *ON
064500000000 E02 C                   END
064600000000     C*
064700000000     C                   SETON                                        70
064800020812     C                   EVAL      *IN30 = HIN30
064900000000     C                   UPDATE    DSF2
065000020812     C                   EVAL      *IN30 = *OFF
065100000000     C*
065200000000     C                   READC     DSF2                                   21
065300000000 E01 C                   END
065400020809
065500000000     C     CON100        TAG
065600020809
065700000000     C     NRR1          CABEQ     0             CON200
065800000000     C*
065900000000     C*...Calcola tot. sfl-1.
066000000000     C*
066100000000     C                   READC     DSF1                                   21
066200000000     C   21              SETOFF                                           61
066300000000     C*
066400000000     C     *IN21         DOWEQ     '0'
066500020809
066600020809     C                   EVAL      RicSN = %TRIML(RicSN)
066700020809
066800020809     C                   SELECT
066900020809
067000020812     ** Opzione 21=Contabilizzazione movimento.
067100020809     C                   WHEN      RicSN = '21'
067200020809     C                   EXSR      CtbBmb
067300020812     C                   CLEAR                   RicSN
067400020812     C                   EVAL      *IN21 = (Ret251 = '1')
067500020812
067600020812     ** Opzione  5=Visualizzazione movimento.
067700020812     C                   WHEN      RicSN = '5'
067800020812     C                   EXSR      CalNdCT41R
067900020812     C                   CLEAR                   RicSN
068000020812     C                   EVAL      *IN21 = (RetT41 = '1')
068100020809
068200020809     C                   WHEN      RicSN <> *BLANK
068300020813     C                   Z-ADD     1             A
068400000000     C     RICSN         LOOKUP    SKI(A)                                 10
068500000000     C*
068600000000     C*
068700910213     C     LET(A)        IFEQ      *BLANK
068800020809     C                   MOVEL     RICSN         LET(A)
068900000000     C                   END
069000000000     C*
069100000000     C                   ADD       IMPORT        SKB(A)
069200990827     C                   eval      dva(a) = dvam
069300031002     C                   EVAL      WrkSelBMB = *ON
069400000000     C*
069500020809 E02 C                   ENDSL
069600000000     C*
069700020812     C                   EVAL      *IN30 = HIN30
069800020812     C                   EVAL      *IN32 = HIN32
069900020812     C                   EVAL      *IN41 = HIN41
070000000000     C                   SETON                                        70
070100000000     C                   UPDATE    DSF1
070200020812     C                   EVAL      *IN30 = *OFF
070300020812     C                   EVAL      *IN32 = *OFF
070400020812     C                   EVAL      *IN41 = *OFF
070500020812     C  N21              READC     DSF1                                   21
070600020809     C                   ENDDO
070700000000     C*
070800000000     C     CON200        TAG
070900000000     C*
071000031002     C                   EXSR      ErrSelBmv
071100031002
071200000000     C*----------------------------------------------------*
071300000000     C/EJECT
071400000000     C*----------------------------------------------------*
071500000000     C*
071600000000     C                   SETOFF                                       38
071700000000     C*
071800000000     C*...Controllo sfl-1.
071900000000     C*
072000910604MM   C                   MOVE      *ZERO         DOP
072100910604MM   C                   MOVE      *ZERO         FLG
072200000000     C     NRR1          CABEQ     0             CON300
072300000000     C                   DO        *HIVAL
072400000000     C*
072500000000     C                   READC     DSF1                                   21
072600000000     C*
072700910604 .   C  N21              DO
072800020809
072900020809     C                   EVAL      RicSN = %TRIML(RicSN)
073000020809
073100910604MM   C     RICSN         IFNE      *BLANK
073200020812MM   C     RICSN         ANDNE     '21'
073300020812MM   C     RICSN         ANDNE     '5'
073400910604MM   C                   Z-ADD     1             K
073500910604MM   C     RICSN         LOOKUP    SKI(K)                                 10
073600910604MM   C                   Z-ADD     DOPB          DOP(K)
073700910604MM   C                   Z-ADD     DOPB          FLG(K)
073800910604MM   C                   END
073900000000     C*
074000000000     C*...Se e' automatica faccio i controlli di validita'-
074100000000     C*
074200000000     C*...controllo di quadratura tra banca e contabilita'.
074300000000     C     RICSN         IFEQ      ' '
074400020812     C     RICSN         OREQ      '21'
074500020812     C     RICSN         OREQ      '5'
074600020812     C                   EVAL      HIN30 = *OFF
074700020812     C                   SETOFF                                       31
074800000000     C                   ELSE
074900020813     C                   Z-ADD     1             K
075000000000     C     RICSN         LOOKUP    SKI(K)                                 10
075100000000     C*
075200000000 IF2 C     SKB(K)        IFEQ      SKM(K)
075300020812     C                   EVAL      HIN30 = *OFF
075400020812     C                   SETOFF                                       31
075500000000  X2 C                   ELSE
075600020812     C                   EVAL      HIN32 = *OFF
075700020812     C                   EVAL      HIN41 = *OFF
075800020812     C                   EVAL      HIN30 = *ON
075900020812     C                   SETON                                        3150
076000000000     C     SKB(K)        SUB       SKM(K)        SQUA
076100000000     C                   MOVEL     SKI(K)        $TIP
076200000000     C                   SETON                                        38
076300000000     C                   MOVE      NRR1          RECNBR
076400000000     C                   END
076500000000 E02 C                   END
076600000000     C*
076700000000     C                   SETON                                        70
076800020812     C                   EVAL      *IN30 = HIN30
076900020812     C                   EVAL      *IN32 = HIN32
077000020812     C                   EVAL      *IN41 = HIN41
077100000000     C                   UPDATE    DSF1
077200020812     C                   EVAL      *IN30 = *OFF
077300020812     C                   EVAL      *IN32 = *OFF
077400020812     C                   EVAL      *IN41 = *OFF
077500000000     C*
077600000000 .   C                   END
077700000000     C*
077800000000     C  N21              END
077900000000     C*
078000000000     C     CON300        TAG
078100000000     C*
078200000000     C*...Controllo sfl-2.
078300000000     C*
078400000000     C     NRR2          CABEQ     0             CON400
078500000000     C                   SETON                                        7778
078600000000     C                   DO        *HIVAL
078700000000     C*
078800000000     C                   READC     DSF2                                   21
078900000000     C*
079000000000 .   C  N21              DO
079100020809
079200020809     C                   EVAL      RicSN = %TRIML(RicSN)
079300020809
079400910604MM   C     RICSN         IFNE      *BLANK
079500910604MM   C                   Z-ADD     1             K
079600910604MM   C     RICSN         LOOKUP    SKI(K)                                 10
079700910604MM   C     DOPM          IFLT      DOP(K)
079800910604MM   C     DOP(K)        OREQ      0
079900910604MM   C                   Z-ADD     DOPM          DOP(K)
080000910604MM   C                   Z-ADD     DOPM          FLG(K)
080100910604MM   C                   END
080200910604MM   C                   END
080300000000     C*
080400000000     C*...Se e' automatica faccio i controlli di validita'-
080500000000     C*
080600000000     C*...controllo di quadratura tra banca e contabilita'.
080700000000 IF2 C     RICSN         IFEQ      ' '
080800020812     C                   EVAL      HIN30 = *OFF
080900020812     C                   SETOFF                                       31
081000000000     C                   ELSE
081100020813     C                   Z-ADD     1             K
081200000000     C     RICSN         LOOKUP    SKI(K)                                 10
081300000000     C*
081400000000 IF3 C     SKB(K)        IFEQ      SKM(K)
081500020812     C                   EVAL      HIN30 = *OFF
081600020812     C                   SETOFF                                       31
081700000000  X3 C                   ELSE
081800020812     C                   EVAL      HIN30 = *ON
081900020812     C                   SETON                                        3150
082000000000     C     SKB(K)        SUB       SKM(K)        SQUA
082100000000     C                   MOVEL     SKI(K)        $TIP
082200000000     C                   SETON                                        38
082300000000     C                   MOVE      NRR2          RECNB2
082400000000 E03 C                   END
082500000000 E02 C                   END
082600000000     C*
082700000000     C                   SETON                                        70
082800020812     C                   EVAL      *IN30 = HIN30
082900000000     C                   UPDATE    DSF2
083000020812     C                   EVAL      *IN30 = *OFF
083100000000     C*
083200000000 .   C                   END
083300000000     C*
083400000000     C  N21              END
083500000000     C     CON400        TAG
083600000000     C*
083700000000     C                   SETOFF                                       7778
083800000000     C*
083900000000     C                   ENDSR
084000000000     C*
084100000000     C*---------------------------------------------------------------*
084200000000     C/EJECT
084300000000     C*
084400000000     C*---------------------------------------------------------------*---
084500000000     C*  CARSB1:   CARICA SUBFILE DELLE BANCHE                        *
084600000000     C*---------------------------------------------------------------*
084700000000     C*
084800000000     C     CARSB1        BEGSR
084900000000     C*
085000000000     C                   MOVEL     *BLANK        PIENO
085100000000     C                   Z-ADD     0             SQUA
085200000000     C                   Z-ADD     NUMREC        NRR1
085300020812     C                   EVAL      HIN30 = *OFF
085400020812     C                   EVAL      HIN32 = *OFF
085500020812     C                   EVAL      HIN41 = *OFF
085600000000     C*
085700000000     C*°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°*
085800000000DO2  C                   DO        7
085900000000     C*
086000990825     C                   if        bmbdare = 1
086100990825     C                   eval      da = 'D'
086200990901     C                   Z-ADD     bmbimporto    IMPORT
086300990825     C                   else
086400990825     C                   eval      da = 'A'
086500990901     C                   Z-SUB     bmbimporto    IMPORT
086600990825     C                   endif
086700000000     C                   MOVEL     *BLANK        RICSN
086800000000     C*
086900000000     C*...Carico riga banca.
087000000000     C*
087100910213     C                   Z-ADD     NRRBMB        H1NRR
087200021025     C                   EVAL      H1SysN = BmbSysN
087300021025     C                   EVAL      H1Num = BmbNum
087400021025     C                   EVAL      H1NrRiga = BmbNrRiga
087500990824     C*
087600990825     C     *dmy          move      bmbdtope      dopb
087700000000     C*
087800990825     C     *dmy          move      bmbdtval      dvab
087900000000     C*
088000990825     C                   MOVEL     bmbcauban     CAUB
088100990830     C* Visualizzo la nota ricevuta dalla banca
088200990830     C* cercando di scartare gli spazi iniziali.
088300990830     C                   clear                   desb
088400990830     C                   clear                   wrkdescr
088500990901     C                   eval      desb = %trim(bmbcauban)
088600990901     C     desb          cat       ' ':0         desb
088700990830     C                   if        bmbnrdoc > 0
088800990901     C                   move      bmbnrdoc      nrdocchr
088900990901     C     desb          cat       nrdocchr:1    desb
089000990901     C     desb          cat       ' ':0         desb
089100990830     C                   endif
089200990830     C                   if        bmbdescr <> *blanks
089300990901     C                   eval      wrkdescr = %trim(bmbdescr)
089400990830     C                   if        desb = *blanks
089500990830     C                   eval      desb = wrkdescr
089600990830     C                   else
089700990830     C     desb          cat       wrkdescr:1    desb
089800990830     C                   endif
089900990830     C                   endif
090000020905     ** Tolgo i caratteri non validi dalla descrizione movimento.
090100020905     C                   EVAL      I = 1
090200020905     C                   DOU       I > %SIZE(DesB)
090300020905     C                   IF        %SUBST(DesB:I:1) < *BLANK
090400020905     C                   EVAL      %SUBST(DesB:I:1) = *BLANK
090500020905     C                   ENDIF
090600020905     C                   EVAL      I = I + 1
090700020905     C                   ENDDO
090800000000     C*
090900000000     C                   ADD       1             NRR1
091000000000     C                   ADD       1             NUMREC            4 0
091100000000     C*
091200000000     C                   SETOFF                                       70
091300000000     C                   WRITE     DSF1
091400000000     C                   MOVEL     '1'           PIENO             1
091500990827     C*
091600000000?    C     R16           TAG                                                                 SF
091700000000     C*
091800990825     C     k03bmb03      READE     ndbmb000                               90
091900000000     C*
092000990902     C                   if        not *in90
092100990902     C                             and
092200990902     C                             (bmbstatus <> *blanks
092300990902     C                             or
092400990902     C                             bmbdtope < d1dopiiso
092500990902     C                             or
092600990915     C                             bmbdtope > d1dopfiso
092700990915     C                             or
092800990915     C                             bmbann = 1)
092900990902     C                   goto      r16
093000990902     C                   endif
093100000000     C*
093200000000E02  C  N90              END
093300000000     C*
093400000000     C*°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°*
093500000000     C*
093600000000     C*____________________________________________________*
093700000000     C*
093800000000     C                   Z-ADD     1             NUMPAG                         *
093900000000     C     NUMREC        DIV       7             NUMPAG                         *
094000000000     C                   MVR                     RESTO             4 0          *
094100000000     C     RESTO         IFNE      0                                            *
094200910213     C                   ADD       1             NUMPAG                         *
094300000000     C                   END                                                    *
094400910213     C                   Z-ADD     NUMPAG        PAGINA
094500000000     C*                                                    *
094600000000     C                   ENDSR
094700000000     C*---------------------------------------------------------------*
094800000000     C/EJECT
094900000000     C*---------------------------------------------------------------*---
095000000000     C*  CARSB2:   CARICA SUBFILE DELLA CONTABILITA'                  *
095100000000     C*---------------------------------------------------------------*
095200000000     C     CARSB2        BEGSR
095300000000     C*
095400000000     C                   MOVEL     *BLANK        PIEN1
095500000000     C                   Z-ADD     NUMRE2        NRR2
095600020812     C                   EVAL      HIN30 = *OFF
095700000000     C*
095800020812DO2  C                   DO        7
095900000000     C*
096000990825     C                   if        bmvdare = 1
096100990825     C                   eval      da = 'D'
096200990901     C                   Z-ADD     bmvimporto    IMPORT
096300990825     C                   else
096400990825     C                   eval      da = 'A'
096500990901     C                   Z-SUB     bmvimporto    IMPORT
096600990825     C                   endif
096700000000     C                   MOVEL     *BLANK        RICSN
096800000000     C*
096900910213     C                   Z-ADD     NRRBMV        H2NRR
097000990825     C                   Z-ADD     bmvnrasreg    H2NCB
097100990825     C                   eval      h2sys = bmvsys
097200990825     C                   eval      h2nrrigam = bmvnrrigam
097300990825     C*
097400990825     C     *dmy          move      bmvdtope      hdopm
097500990826     C     *dmy          move      bmvdtope      dopm
097600000000     C*
097700990825     C     *dmy          move      bmvdtval      hdvam
097800990826     C     *dmy          move      bmvdtval      dvam
097900990824
098000990824     C**********************************************************************
098100990824     C                   reset                   Nd017Ds
098200990824     C                   Eval      LenOut = %Size(ND017DS)
098300990824     C                   callb     'NDDVABCO'
098400990824     C                   PARM      '01'          Operazione
098500990824     C                   PARM                    Xscsoc
098600990824     C                   PARM                    rapv
098700990824     C                   PARM                    lineav
098800990825     C                   PARM                    bmvcauban
098900990825     C                   PARM                    bmvdtope
099000990824     C                   PARM      'ND017DS'     StrutturaO
099100990824     C                   PARM                    LenOut
099200990824     C                   PARM      *off          Esito
099300990830     C     nd017ds       PARM                    Output
099400990824     C                   PARM                    XscTesRid
099500990824     C                   PARM                    NoBCO
099600990824     C**********************************************************************
099700020812     C                   EVAL      DesM = BmvCauBan
099800990824
099900000000     C                   SETOFF                                       8081
100000990830     C                   eval      movsys = bmvsys
100100990830     C                   eval      movnrasreg = bmvnrasreg
100200990830     C                   eval      movnrrigam = bmvnrrigam
100300990830     C     k03mov01      chain     ndmov01l
100400990830     C                   if        %found
100500990830     C                   eval      wrknaschr = %subst(movdesbrev:11:10)
100600020812     C                   testn                   wrknaschr            21
100700020812     C                   if        *in21
100800020812     C                   EVAL      DesM = %TRIMR(DesM) + ' '
100900020812     C                             + wrknaschr
101000020812     C                   endif
101100990830     C                   else
101200990830     C                   eval      wrknaschr = *zeros
101300990830     C                   endif
101400020812
101500020812     C                   EVAL      DesM = %TRIMR(DesM) + ' '
101600020812     C                             + desbrev017
101700990830     C**********************************************************************
101800990830
101900000000     C                   ADD       1             NRR2
102000000000     C                   ADD       1             NUMRE2            4 0
102100000000     C*
102200000000     C                   SETOFF                                           70
102300000000     C                   WRITE     DSF2
102400000000     C                   MOVEL     '1'           PIEN1             1
102500000000     C*
102600000000?    C     R182          TAG                                                    *
102700000000     C*
102800990825     C     KEY           READE     ndbmv000                               99
102900000000     C*
103000990830     C  N99bmvtpregz     cabne     'D'           R182
103100990825     C  N99bmvstatus     CABNE     ' '           R182
103200990902     C                   if        not *in99
103300990902     C                             and (bmvdtope < d1dopiiso
103400990902     C                                 or bmvdtope > d1dopfiso)
103500990902     C                   goto      r182
103600990902     C                   endif
103700000000     C*
103800000000E02  C  N99              END
103900000000     C*
104000000000     C*____________________________________________________*
104100000000     C*
104200000000     C*
104300000000     C*                                                    *
104400000000     C                   Z-ADD     1             NUMPA2                         *
104500020812     C     NUMRE2        DIV       7             NUMPA2                         *
104600000000     C                   MVR                     REST2             4 0          *
104700000000     C     REST2         IFNE      0                                            *
104800000000     C                   ADD       1             NUMPA2                         *
104900000000     C                   END                                                    *
105000910213     C                   Z-ADD     NUMPA2        PAGIN2
105100000000     C*
105200000000     C                   ENDSR
105300000000     C*
105400000000     C*---------------------------------------------------------------*
105500920302     C/EJECT
105600000000     C*---------------------------------------------------------------*---
105700000000     C*            AGGIORNA PROPOSTE
105800000000     C*---------------------------------------------------------------*---
105900000000     C     AGGIOR        BEGSR
106000000119     C*
106100990903     C                   clear                   bpr
106200000119     C                   clear                   xnmnum
106300990903     C                   eval      a = 1
106400000119     C* Scorro tutta la schiera delle lettere
106500000119     C* perchè potrebbero esserci dei buchi.
106600000119     C                   dow       a <= 26
106700000119     C                   if        let(a) <> *blank
106800000119     C* Prelevo numeri riconciliazione.
106900000119     C                   if        xnmnum = 0
107000990903     C                   Eval      XNMRIC = '4'
107100990903     C                   EXSR      callxnmr
107200990903     C                   else
107300990903     C                   add       1             xnmnum
107400990903     C                   endif
107500990903     C                   Z-Add     XNMNUM        bpr(a)
107600000119     C*
107700000119     C                   endif
107800990903     C                   add       1             a
107900990903     C                   enddo
108000000119     C* Se il numeratore è pieno significa che c'è almeno una riconciliazion
108100000119     C                   if        xnmnum > 0
108200000119     C* Aggiorno numeratore con l'ultimo numero usato.
108300990903     C                   Eval      XNMRIC = '5'
108400990903     C                   EXSR      callxnmr
108500000000     C*...Aggiorno il sfl-1.
108600000000     C     NRR1          CABEQ     0             AGG100
108700990903     C                   clear                   nmr
108800000000     C*
108900000000 1   C                   DO        *HIVAL        NRR1
109000000000     C*
109100000000     C     NRR1          CHAIN     DSF1                               22
109200000000     C*
109300000000 2   C  N22              DO
109400000000 3   C     RICSN         IFNE      ' '
109500020812 3   C     RICSN         ANDNE     '21'
109600020812 3   C     RICSN         ANDNE     '5'
109700990824     C     H1NRR         CHAIN     ndbmb                              44
109800910213     C  N44              DO
109900910213     c*
110000910213     c* imposta progressivo riconciliazione
110100990903     c*
110200020813     C                   Z-ADD     1             A
110300990903     C     RICSN         LOOKUP    let(A)                                 10
110400990903     C                   add       1             nmr(a)
110500990826     C                   Z-ADD     BPR(A)        BMBnrric
110600990825     C                   MOVEL     '1'           bmbstatus
110700000111     C                   if        bmbnrric > 0
110800990824     C                   UPDATE    ndbmb
110900021128      *
111000000111     C                   else
111100000111     C                   unlock    ndbmb00f
111200000111     C                   eval      sndpm03 = 'ATTENZIONE:'
111300000111     C                             + ' aggiornamento di NDBMB00F non eseguito.'
111400000111     C                             + ' Avvisare il CED e rimanere fermi su'
111500000111     C                             + ' questo messaggio.'
111600000111     C                   exsr      sndpgmmsg
111700000111     C                   endif
111800000000     C                   END
111900000000 3E  C                   END
112000000000 2E  C                   END
112100000000     C*
112200000000 1E  C  N22              END
112300000000     C*
112400000000     C     AGG100        TAG
112500000000     C     NRR2          CABEQ     0             AGG200
112600000000     C*
112700000000     C*...Aggiorno il sfl-2.
112800000000     C*
112900000000 1   C                   DO        *HIVAL        NRR2
113000000000     C*
113100000000     C     NRR2          CHAIN     DSF2                               22
113200000000     C*
113300000111 2   C     *IN22         IFEQ      *OFF
113400000111 3   C     RICSN         ANDNE     ' '
113500000111     C*
113600000111     C     H2NRR         CHAIN     ndbmv                              44
113700000111     C*
113800000111 3   C     *IN44         IFEQ      *OFF
113900000111     C*
114000990825     C                   MOVEL     '1'           bmvstatus
114100910213     c* imposta progressivo riconciliazione
114200020813     C                   Z-ADD     1             A
114300990903     C     RICSN         LOOKUP    let(A)                                 10
114400990826     C                   Z-Add     bpr(a)        BmvNrTeso
114500910604MC   C                   MOVE      *ZERO         FLG(A)
114600000000     C*
114700000111     C                   if        bmvnrteso > 0
114800000111     C                   UPDATE    ndbmv
114900021128     C*
115000021128     C* a questo punto deve aggiornare tutti i movimenti della catena
115100021128      * aggiorna mov.sede
115200021128     c                   exsr      call361R
115300021128      * aggiorna mov.banca
115400021128     c                   exsr      call362R
115500021128      *
115600000111     C                   else
115700000111     C                   unlock    ndbmv00f
115800000111     C                   eval      sndpm03 = 'ATTENZIONE:'
115900000111     C                             + ' aggiornamento di NDBMV00F non eseguito.'
116000000111     C                             + ' Avvisare il CED e rimanere fermi su'
116100000111     C                             + ' questo messaggio.'
116200000111     C                   exsr      sndpgmmsg
116300000111     C                   endif
116400000111     C*
116500000111 2E  C                   END
116600000000     C*
116700000000 2E  C                   END
116800000000     C*
116900000000 1E  C  N22              END
117000990903
117100910510     C     AGG200        TAG
117200990903
117300990903MC   C* Scrittura sintesi di riconciliazione.
117400990903MC   C* Scrivo solo un record perchè so che i numeri di riconciliazione
117500990903     C* sono consecutivi.
117600990903     C                   eval      bsrnrda = *hival
117700990903     C                   eval      bsrnra  = *loval
117800990903     C                   clear                   bsrnrmov
117900990903     C                   eval      a = 1
118000990903     C* Scorro la schiera delle lettere usate.
118100000119     C* La devo scorrere tutta perchè ci possono essere dei buchi.
118200000119     C                   dow       a <= 26
118300990903     C* Reperisco il numero più basso.
118400990903     C                   if        bpr(a) < bsrnrda
118500020809     C                             and
118600020809     C                             bpr(a) > 0
118700990903     C                   eval      bsrnrda = bpr(a)
118800990903     C                   endif
118900990903     C* Reperisco il numero più alto.
119000990903     C                   if        bpr(a) > bsrnra
119100990903     C                   eval      bsrnra  = bpr(a)
119200990903     C                   endif
119300990903     C* Calcolo il numero dei movimenti riconciliati.
119400990903     C                   add       nmr(a)        bsrnrmov
119500990903     C*
119600990903     C                   add       1             a
119700990903MC   C                   enddo
119800990903     C*
119900990903     C                   eval      bsrsocieta = xscsoc
120000990903MC   C                   eval      bsrbanca = banca016
120100990903     C                   eval      bsrlinea = linea016
120200990903     C                   move      *date         bsrdtric
120300990903     C                   time                    bsrora
120400990903     C                   eval      bsrtipo = 'M'
120500990903     C                   eval      bsrprofilo = knmus
120600990903AM   C                   write     ndbsr000
120700990827     C*
120800990827     C                   commit
120900030929     C*
121000030929     C                   EXSR      ChkQuadratura
121100990830     C* Ricalcolo i saldi.
121200990830     C                   exsr      saldo
121300000119     C*
121400000119     C                   endif
121500000000     C*
121600000000     C                   ENDSR
121700990825
121800990826     C*----------------------------------------------------------
121900021128     C* Aggiorna la catena di movimenti del NDBMV00F
122000990826     C*----------------------------------------------------------
122100021128     C     call361r      BEGSR
122200990826     C*
122300021128     c                   eval      Y36SYS     = BMVSYS
122400021128     c                   eval      Y36NRASREG = BMVNRASREG
122500021128     c                   eval      Y36NRRIGAM = BMVNRRIGAM
122600021128     c                   eval      Y36NRTESO  = BMVNRTESO
122700021128     C*
122800021128     c                   eval      kpjbus = kpjbu
122900021128     c                   clear                   kpjbu
123000021128     c                   eval      kpjbu = Yb361Ds
123100021128     c                   call      'YB361R'
123200021128     c                   parm                    kpjba
123300021128     c                   eval      kpjbu = kpjbus
123400021128     C*
123500021128     C                   ENDSR
123600021128
123700021128     C*----------------------------------------------------------
123800021128     C* Aggiorna la catena di movimenti del NDBMB00F
123900021128     C*----------------------------------------------------------
124000021128     C     call362r      BEGSR
124100021128     C*
124200021128     c                   eval      Y36SYS     = BMVSYS
124300021128     c                   eval      Y36NRASREG = BMVNRASREG
124400021128     c                   eval      Y36NRRIGAM = BMVNRRIGAM
124500021128     c                   eval      Y36NRTESO  = BMVNRTESO
124600021128     C*
124700021128     c                   eval      kpjbus = kpjbu
124800021128     c                   clear                   kpjbu
124900021128     c                   eval      kpjbu = Yb361Ds
125000021128     c                   call      'YB362R'
125100021128     c                   parm                    kpjba
125200021128     c                   eval      kpjbu = kpjbus
125300021128     C*
125400021128     C*
125500021128     C                   ENDSR
125600021128
125700021128     C*----------------------------------------------------------
125800021128     C* Preleva un numeratore attraverso la routine XNMR
125900021128     C*----------------------------------------------------------
126000021128     C     callxnmr      BEGSR
126100021128     C*
126200000111     C                   Eval      XNMSER = 'NRHB'
126300990826     C                   Eval      XNMSOC = xscsoc
126400990826     C                   Eval      XNMUNI = *Blank
126500990826     C                   Eval      XNMCTB = *Blank
126600990826     C                   Eval      XNMKEY = *Blank
126700990826     C                   Eval      XNMCMT = *OFF
126800990826     C                   Eval      XNMIVA = *OFF
126900990826     C                   Eval      XNMREG = *OFF
127000990826     C                   Move      *date         XNMDAG
127100990826     C*
127200990826     C                   CALLB     'XNMR'        PNMR
127300990826     C*
127400990826     C                   ENDSR
127500990826
127600990826     C     *inzsr        begsr
127700990826     C*
127800990826     C     *ENTRY        PLIST
127900990826     C                   PARM                    KPJBA
128000990827     C*
128100990827     C* Attiva commitment control
128200990827     C*
128300990827    >C                   CALLB     'XSTRCMT'
128400990827    >C                   open      ndbmv00f
128500990827    >C                   open      ndbmb00f
128600990827    >C                   open      ndbsr00f
128700990826     C*
128800990826     C                   CALLB     'XSOC'
128900990826     C                   PARM      'SOC001'      TIPXSC            6
129000990826     C                   PARM                    SOCXSC            3
129100990826     C                   PARM                    CDSXSC            9 0
129200990826     C                   PARM                    MODXSC            3
129300990826     C                   PARM                    RTNXSC            1
129400990826     C     xsoc001ds     PARM                    xsocds         1000
129500990826     C                   PARM                    KPJBA
129600990826     C     RTNXSC        IFEQ      *ON
129700990826     C                   SETON                                        LR
129800990826     C                   RETURN
129900990826     C                   ENDIF
130000990826     C*-
130100990826     C                   MOVEL     xscdsi        RSUT
130200020812     C*
130300020812     C                   EXSR      RepAut
130400990826     C*-
130500990830     C     k03mov01      klist
130600990830     C                   kfld                    movsys
130700990830     C                   kfld                    movnrasreg
130800990830     C                   kfld                    movnrrigam
130900020813     C     k02mov01      klist
131000020813     C                   kfld                    movsys
131100020813     C                   kfld                    movnrasreg
131200990830     C*
131300990826     C     KEY           KLIST
131400990826     C                   KFLD                    xscsoc
131500990826     C                   KFLD                    bmvctb
131600990826     C                   KFLD                    RAPV
131700990826     C                   KFLD                    lineav
131800990826     C                   eval      bmvctb = 'CG'
131900990826     C     k03bmb03      klist
132000990826     C                   kfld                    xscsoc
132100990826     C                   kfld                    rapv
132200990826     C                   kfld                    lineav
132300990826     C*
132400990826     C*--- CHIAVE PER UPDAT FILE MOVIMENTI
132500990826     C*
132600990826     C*
132700990826     C*--- CHIAVE PER MOVIMENTI CONTAB. ndbmv000  (READ)
132800990826     C*
132900990830     C     k05bmv03      KLIST
133000990826     C                   KFLD                    xscsoc
133100990826     C                   KFLD                    bmvctb
133200990826     C                   KFLD                    RAPV
133300990826     C                   KFLD                    lineav
133400990830     C                   kfld                    bmvdtope
133500990830     C     k04bmv03      KLIST
133600990830     C                   KFLD                    xscsoc
133700990830     C                   KFLD                    bmvctb
133800990830     C                   KFLD                    RAPV
133900990830     C                   KFLD                    lineav
134000990826     C                   eval      bmvctb = 'CG'
134100990826     C* PLIST per XNUM
134200990826     C     PNMR          PLIST
134300990826     C                   PARM                    XNMRIC            1
134400990826     C                   PARM                    XNMSOC            3
134500990826     C                   PARM                    XNMUNI            8
134600990826     C                   PARM                    XNMCTB            2
134700990826     C                   PARM                    XNMKEY            8
134800990826     C                   PARM                    XNMSER            4
134900990826     C                   PARM                    XNMCMT            1
135000990903     C                   PARM                    XNMDAT
135100990826     C                   PARM                    XNMIVA            1
135200990826     C                   PARM                    XNMREG            1
135300990826     C                   PARM                    XNMERR            1
135400990826     C                   PARM                    XNMNUM            9 0
135500990826     C                   PARM                    XNMAUT            1
135600990903     C                   PARM                    XNMDAG
135700990826     C*
135800990826     C                   endsr
135900990827
136000990827     C     callana399    begsr
136100990827     C                   reset                   ana399ds
136200990827     C                   eval      A39societa = XSCSOC
136300990827     C                   eval      A39ctb = 'CG'
136400990827     C                   eval      A39kcc = kcc016
136500990827     C                   eval      A39ksc = ksc016
136600990827     C                   movel     ana399ds      kpjbu
136700990827     C                   CALL(e)   'ANA399R'
136800990827     C                   PARM                    KPJBA
136900990827     C                   endsr
137000990825
137100000111     C     sndpgmmsg     begsr
137200000111     C*
137300000111     C                   reset                   qusec
137400000111     C                   eval      qusbprv = %size(qusec)
137500000111     C                   call      'QMHSNDPM'
137600000111     C                   parm                    sndpm01
137700000111     C                   parm                    sndpm02
137800000111     C                   parm                    sndpm03
137900000111     C                   parm                    sndpm04
138000000111     C                   parm                    sndpm05
138100000111     C                   parm                    sndpm06
138200000111     C                   parm                    sndpm07
138300000111     C                   parm                    sndpm08
138400000111     C                   parm                    qusec
138500000111     C*
138600000111     C                   endsr
138700020809
138800020809     ***********************************************************************
138900020809     **
139000020809     ** Opzione 21 contabilizzo movimento banca.
139100020809     **
139200020809     ***********************************************************************
139300020809     C     CtbBmb        BEGSR
139400020809
139500020812     ** Controllo autorizzazione all'opzione 21=Contabilizza.
139600020812     C                   IF        TesCtbRc = *OFF
139700020812     C                   EVAL      HIN32 = *ON
139800020812     C                   EVAL      *IN33 = *ON
139900020812     C                   EVAL      *IN50 = *ON
140000020812     C                   LEAVESR
140100020812     C                   ENDIF
140200020812
140300020809     C     H1NRR         CHAIN(N)  ndbmb
140400020812
140500020813     ** Se il movimento risulta già contabilizzato
140600020813     ** e la registrazione esiste,
140700020813     ** errore.
140800020812     C                   IF        BmbNrAsReg > 0
140900020813     C                   eval      movsys = bmbsys
141000020813     C                   eval      movnrasreg = bmbnrasreg
141100020813     C     k02mov01      setll     ndmov01l
141200020813     C                   IF        %EQUAL
141300020812     C                   EVAL      HIN32 = *ON
141400020812     C                   EVAL      *IN39 = *ON
141500020812     C                   EVAL      *IN50 = *ON
141600020812     C                   LEAVESR
141700020813     C                   ENDIF
141800020812     C                   ENDIF
141900020812
142000020809     c                   Reset                   Ndc250Ds
142100020809     c                   movel     BmbSocieta    Societa250
142200020809     c                   movel     BmbBanca      Banca250
142300020809     c                   movel     BmbLinea      Linea250
142400020809     c                   movel     BmbDtCar      DtCar250
142500020809     c                   movel     BmbDtInv      DtInvio250
142600020809     C                   MOVE      BmbDtOpe      DtOpe250
142700020809     C                   MOVE      BmbDtVal      DtVal250
142800020809     c                   movel     BmbCauBan     CauBan250
142900020809     c                   movel     BmbDescr      Descr250
143000020809     c                   if        BmbDare = 1
143100020809     c                   movel     'D'           Segno250
143200020809     c                   else
143300020809     c                   movel     'A'           Segno250
143400020809     c                   endif
143500020809     c                   Z-add     BmbImportD    ImportD250
143600020809     c                   movel     BmbDivisa     Divisa250
143700020809     c                   Z-add     BmbCambio     Cambio250
143800020809     c                   movel     *LOVAL        DtDoc250
143900020809     c                   Z-add     BmbNrDoc      NrDoc250
144000020809     C                   Clear                   KPJBU
144100020809     C                   CALL      'NDC250R'
144200020809     C                   PARM                    KPJBA
144300020809     C                   PARM                    Ndc250Ds
144400020809     C* ritorno da PGM
144500020809     C                   EXSR      RTN250
144600020809     C*
144700020809
144800020809     C                   ENDSR
144900020809
145000020809     C************************************************************
145100020809     C* GESTIONE RITORNO da contabilità
145200020809     C************************************************************
145300020809    >C     RTN250        BEGSR
145400020809     C*
145500020809     C* funzione non eseguibile per errore :
145600020809     C*
145700020809     C                   SELECT
145800020809     C* 1 = funzione richiamata chiusa in errore perchè non trovata
145900020809     C* tabella 'K11'.
146000020809    >C     ERR250        WHENEQ    '1'
146100020812     C                   EVAL      HIN32 = *ON
146200020812     C                   EVAL      HIN41 = *ON
146300020812     C                   SETON                                        4250
146400020809     C* 2 = funzione richiamata chiusa in errore perchè codice banca errato
146500020809     C     ERR250        WHENEQ    '2'
146600020809     C*                  MOVE      *ON           $ESCI                          sospese READC
146700020809     C*                  SETON                                        7099      emiss.SFLMSGID
146800020809     c                   other
146900020809     c*  Se ok chiama la videata di proposta dati di registrazione
147000020809     c                   exsr      Rie251ds
147100020809     C                   Clear                   KPJBU
147200020809     c                   Call      'NDC251R'
147300020809     C                   PARM                    KPJBA
147400020809     C                   PARM                    Ndc251Ds
147500020809C1238C                   PARM                    TesDCtRC
147600020809     c                   Exsr      Rtn251
147700020809    >C*  eventualmente gestire altri codici di errore
147800020809     C*
147900020809     C                   ENDSL
148000020809     C*
148100020809     C                   ENDSR
148200020809
148300020809     C************************************************************
148400020809     C* RIEMPIE ds per proposta
148500020809     C************************************************************
148600020809    >C     Rie251Ds      BEGSR
148700020809     C*
148800020809     C                   reset                   ndc251ds
148900020809     c                   eval      opz251 = '02'
149000020809     c                   eval      societa251 = xscsoc
149100020809     c                   eval      CauBan251  = BmbCauban
149200020809     c                   eval      CauCont251 = CauCont250
149300020809     c                   eval      Classe251  = Classe250
149400020809     c                   eval      Unita251   = Unita250
149500020809     c                   eval      Ctb251     = Ctb250
149600020809     c                   eval      TpRegz251  = Tpregz250
149700020809     c                   eval      DtReg251   = DtReg250
149800020809     c                   eval      NrReg251   = NrReg250
149900020809     c                   eval      SerReg251  = SerReg250
150000020809     c                   eval      DtDoc251   = DtDoc250
150100020809     c                   eval      NrDoc251   = NrDoc250
150200020809     c                   eval      SerDoc251  = SerDoc250
150300020809     c                   eval      Divisa251  = Divisa250
150400020809     c                   eval      Cambio251  = Cambio250
150500020809     c                   eval      Descr251   = Descr250
150600020809     c                   eval      Banca251   = Banca250
150700020809     c                   eval      Linea251   = Linea250
150800020809     c                   eval      kcc251     = kcc250
150900020809     c                   eval      ksc251     = ksc250
151000020809     c                   eval      CausRi1251 = CausRi1250
151100020809     c                   eval      Dtope251   = Dtope250
151200020809     c                   eval      Dtval251   = Dtval250
151300020809     c                   eval      CauTeso251 = CauTeso250
151400020809     c                   eval      Segno251   = Segno250
151500020809     c                   eval      Impord251 = Importd250
151600020809     c                   eval      Import251 = Importo250
151700020809     c                   eval      NrTeso251 = NrTeso250
151800020809     c                   eval      CausRi2251 = Causri2250
151900020809     c                   eval      kcccpu251  = kcccpu250
152000020809     c                   eval      ksccpu251  = ksccpu250
152100020809     C                   eval      wdw251     =  '1'
152200020809D0347C                   eval      AggHB251   =  *off
152300020809     c*
152400020809     c                   endsr
152500020809
152600020809     C************************************************************
152700020809     C* GESTIONE RITORNO da proposta registrazione
152800020809     C************************************************************
152900020809    >C     RTN251        BEGSR
153000020809     C*
153100020809     C* modo di ritorno
153200020809     C*
153300020809     C                   SELECT
153400020809     C* questi modi di utilizzo dei valori di ritorno dal
153500020809     C* pgm di manutenzione rcd di anagrafica sono delle
153600020809     C* proposte, normalmente sempre valide, ma modificabili
153700020809     C* per situazioni particolari
153800020809     C* 1 = F3
153900020809    >C     Ret251        WHENEQ    '1'
154000020809     C* 2 = F12
154100020809    >C     RET251        WHENEQ    '2'
154200020809     C* se il pgm di manutenzione ha prima una guida, è necessario anche
154300020809     c* questo test, altrimenti prevale il retT41 e il sfl non viene
154400020809     C* ricaricato
154500020809     C     OPR251        ANDNE     '1'
154600020809     C*
154700020809     C                   ENDSL
154800020809     c*
154900020809     c*   Chiama programma di contabilizzazione se digitato F6
155000020809     c*
155100020809     C     OPR251        ifeq      '1'
155200020809     c                   exsr      Rie252ds
155300020809     C                   Clear                   KPJBU
155400020809     c                   Call      'NDC252R'
155500020809     C                   PARM                    KPJBA
155600020809     C                   PARM                    Ndc252Ds
155700020809     c                   Exsr      Rtn252
155800020809     c                   endif
155900020809     c*
156000020809     C                   MOVE      '0'           Wrkwdw251
156100020809     C*
156200020809     C                   ENDSR
156300020809
156400020809     C************************************************************
156500020809     C* RIEMPIE DS PER contabilizzazione
156600020809     C************************************************************
156700020809    >C     RIE252Ds      BEGSR
156800020809     C*
156900020809     C                   reset                   ndc252ds
157000020809     c                   eval      opz252     = '02'
157100020809     c                   eval      societa252 = xscsoc
157200020809     c                   eval      CauCont252 = CauCont251
157300020809     c                   eval      Classe252  = Classe251
157400020809     c                   eval      Unita252   = Unita251
157500020809     c                   eval      Ctb252     = Ctb251
157600020809     c                   eval      TpRegz252  = Tpregz251
157700020809     c                   eval      DtReg252   = DtReg251
157800020809     c                   eval      NrReg252   = NrReg251
157900020809     c                   eval      SerReg252  = SerReg251
158000020809     c                   eval      DtDoc252   = DtDoc251
158100020809     c                   eval      NrDoc252   = NrDoc251
158200020809     c                   eval      SerDoc252  = SerDoc251
158300020809     c                   eval      Divisa252  = Divisa251
158400020809     c                   eval      Cambio252  = Cambio251
158500020809     c                   eval      Descr252   = Descr251
158600020809     c                   eval      Banca252   = Banca251
158700020809     c                   eval      Linea252   = Linea251
158800020809     c                   eval      kcc252     = kcc251
158900020809     c                   eval      ksc252     = ksc251
159000020809     c                   eval      CausRi1252 = CausRi1251
159100020809     c                   eval      Dtope252   = Dtope251
159200020809     c                   eval      Dtval252   = Dtval251
159300020809     c                   eval      CauTeso252 = CauTeso251
159400020809     c                   eval      Segno252   = Segno251
159500020809     c                   eval      Importd252= Impord251
159600020809     c                   eval      Importo252= Import251
159700020809     c                   eval      NrTeso252 = NrTeso251
159800020809     c                   eval      CausRi2252 = Causri2251
159900020809     c                   eval      kcccpu252  = kcccpu251
160000020809     c                   eval      ksccpu252  = ksccpu251
160100020809     c                   eval      apripn252  = apripn251
160200020809     c                   eval      spunta252  = spunta251
160300020809D0347c                   eval      AggHB252   = AggHb251
160400020809     C                   MOVEL     TabSalC251    TabSalc252
160500020809    >C                   z-add     BmbSysN       SysN252
160600020809    >C                   z-add     BmbNum        Num252
160700020809    >C                   z-add     BmbNrRiga     NrRiga252
160800020809     C*
160900020809     C                   ENDSR
161000020809
161100020809     C************************************************************
161200020809     C* GESTIONE RITORNO da contabilizzazione
161300020809     C************************************************************
161400020809    >C     RTN252        BEGSR
161500020809     C*
161600020809     C* funzione non eseguibile per errore :
161700020809     C*
161800020809     C                   SELECT
161900020809     C* 1 = funzione richiamata chiusa in errore
162000020809    >C     ERR252        WHENEQ    '1'
162100020809     c                   endsl
162200020809     c*
162300020809     C* Se ok aggiorno bottone
162400020809     c                   if        OPR252 = *ON
162500020809     C*                  exsr      AggS1
162600020809     c                   endif
162700020809     C*
162800020809     C                   ENDSR
162900020809
163000020809     C************************************************************
163100020809     C* REPERIMENTO AUTORIZZAZIONI UTENTE
163200020809     C************************************************************
163300020809     C     REPAUT        BEGSR
163400020809     C*
163500020809     C                   CLEAR                   AUTDS
163600020809     C*
163700020809     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
163800020809     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
163900020809     C                   Z-ADD     0             XCAGRP                         GRUPPO
164000020809     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
164100020809    >C                   MOVEL     'CONTES'      XCAFNC                         FUNZIONE
164200020809    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
164300020809    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
164400020809    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
164500020809     C*
164600020809     C                   CALLB     'XCHKAUT'
164700020809     C                   PARM                    AUTDS
164800020809     C*
164900020809     C* errori/non abilitazione
165000020809     C     XCARTN        IFNE      0
165100020809     C                   EXSR      ENDPGM
165200020809     C                   ELSE
165300020809    >C                   MOVEL     XCADDS        ConTesDS
165400020809     C                   ENDIF
165500020809     C*
165600020809     C                   ENDSR
165700020809
165800020809     C************************************************************
165900020809     C* FINE PROGRAMMA
166000020809     C************************************************************
166100020809     C     ENDPGM        BEGSR
166200020809     C                   SETON                                            LR
166300020809     C                   RETURN
166400020809     C                   ENDSR
166500020812
166600020812     ***********************************************************************
166700020812     **
166800020812     **
166900020812     **
167000020812     ***********************************************************************
167100020812     C     CalNDCT41R    BEGSR
167200020812
167300020812    >C                   RESET                   NdCT41DS
167400021025    >C                   MOVE      XscSoc        SocietaT41
167500020812    >C                   MOVEL     '05'          OPZT41
167600020812    >C                   MOVEL     '0'           CMTT41
167700020812     C* passare nei campi appositi della DS NdCT41DS il valore
167800020812     C* della chiave della VL univoca
167900021025    >C                   Z-add     H1SysN        SysNT41
168000021025    >C                   Z-add     H1Num         NumT41
168100021025    >C                   Z-add     H1NrRiga      NrRigaT41
168200020812     C*
168300020812    >C                   MOVEL     NdCT41DS      KPJBU
168400020812    >C                   CALL      'NDCT41R'
168500020812     C                   PARM                    KPJBA
168600020812    >C                   PARM                    AUTDS
168700020812    >C                   EVAL      NdCT41DS = KPJBU
168800020812
168900020812     C                   ENDSR
169000030929
169100030929     ***********************************************************************
169200030929     **
169300030929     **
169400030929     **
169500030929     ***********************************************************************
169600030929     C     ChkQuadratura BEGSR
169700030929
169800030929     C                   CLEAR                   A
169900030929
170000030929     C                   DO        *HIVAL
170100030929     C
170200030929     C                   EVAL      A = A + 1
170300030929     C
170400030929     C                   IF        A > %ELEM(BPR) OR BPR(A) = 0
170500030929     C                   LEAVE
170600030929     C                   ENDIF
170700030929     C
170800030929     C                   CALL      'YB363R'
170900030929     C                   PARM                    XscSoc
171000030929     C                   PARM                    RapV
171100030929     C                   PARM                    LineaV
171200030929     C                   PARM                    BPR(A)
171300030929     C
171400030929     C                   ENDDO
171500030929
171600030929     C                   ENDSR
171700031002
171800031002     ***********************************************************************
171900031002     **
172000031002     **
172100031002     **
172200031002     ***********************************************************************
172300031002     C     ErrSelBmv     BEGSR
172400031002
172500031002     ** Se non è stato selezionato almeno un movimento BMV errore.
172600031002     C                   IF        WrkSelBmv = *OFF AND WrkSelBmb = *ON
172700031002
172800031002     C                   EVAL      LngTx01 = 'Selezionare almeno un movimento-
172900031002     C                              di società.'
173000031002     C
173100031002     C                   CLEAR                   QUSEC
173200031002     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
173300031002     C
173400031002     C                   CALL      'QUILNGTX'
173500031002     C                   PARM                    LngTx01
173600031002     C                   PARM                    LngTx02
173700031002     C                   PARM                    LngTx03
173800031002     C                   PARM                    LngTx04
173900031002     C                   PARM                    QUSEC
174000031002
174100031002     C                   EVAL      *IN50 = *ON
174200031002
174300031002     C                   ENDIF
174400031002
174500031002     C                   ENDSR
174600000111
174700000000**   INDICI
174800020809A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
