000100080609     H DECEDIT('0,') DATEDIT(*ymd.)
000200940713     H* TNTA68R  *---------------------------------------------------*
000300920529     H*-----------------*                                            *
000400920707     H*             INTERROGAZIONE TARIFFE PARTICOLARI
000500920529     H*--------------------------------------------------------------*
000600940713     FTNTA67D   CF   E             WORKSTN
000700940913     F                                     SFILE(TA67S02:NRR)
000800940913     F                                     SFILE(TA67S07:NR2)
000900990622     FTITPT01L  IF   E           K DISK
001000990622     FTITPD01L  IF   E           K DISK
001100131216     FTipmg01l  IF   E           K DISK
001200920529     FTABEL00F  IF   E           K DISK
001300990622     D WTT             S              2    DIM(50)
001400920609     D TTR             S              1    DIM(50)
001500980504     D DTR             S             10    DIM(50)
001600941114     D TTP             S              1    DIM(50)
001700980520     D ARR             S              1    DIM(50)
001800980520     D RPV             S              1    DIM(50)
001900920612     D PARAM5          DS
002000920612     D  VIDCLI                 2      8  0
002100920612     D  RAGDES                 9     56
002200920612     D  VIDPRG                57     59  0
002300920612     D  VIDCTR                60     62  0
002400920612     D  VIDDCF                63     72
002500920612     D  VIDDCV                73     87
002600920612     D  VIDTES                88    121
002700920619     D  FLG                  122    122
002800921119     D  FLG2                 123    123
002900970325     D  PARFIE               125    125
003000000420     D* FLAG CLIENTE POSTA
003100000420     D  WRKPT                127    127
003200990824     D* CODICE DIVISA
003300990824     D  VICDIV               128    130
003400020201     D* FLAG DECIMALI SI/NO IN BASE ALLA DIVISA
003500020201     D  FLGDEC               131    131
003600020201     D* DATA DECORRENZA TARIFFA
003700020201     D  PARDDT               132    139  0
003800020201     D* TIPO SEWRVIZIO BOLLA
003900020201     D  PARTSP               140    140
004000020201     D* Flag Network
004100020201     D  FLGNET               141    141
004200990907     D WLBDAT          DS
004300990907     D  G02DAT                 1      8  0
004400990907     D  G02INV                 9     16  0
004500990907     D  G02ERR                17     17
004600990907     D  G02TGI                18     22  0
004700920612     D KPJBA         E DS
004800920529     D DS1P          E DS
004900020201     D DSSP          E DS
005000920612     D DSCT          E DS
005100940913     D DSTR          E DS
005200920529     D CNCR80        E DS
005300920529     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005400990217     D DSBS02        E DS                  EXTNAME(TIBS02DS)
005500030718     D TNTA04DS      E DS
005600051012
005700051012     d Kscfil          s              3  0
005800080609      * schiera filiali abilitate al lasciato avviso
005900080609     d sklav           s              3  0 dim(999)
006000080609
006100080609     d xx              s              3  0
006200120321
006300120321       // -?Flag booleani
006400120321     d wFine           s               n   inz(*off)
006500080609
006600080609     d dlav          e ds
006700080609     d tntbeds       e ds                  extname(tntbe00f)
006800120321
006900120321       // -?ds dati passati da TNSB48R - Manca Tariffa
007000120321     d TNSB48ds      e ds
007100120321     d Parm48          s                   like(tnsb48ds)
007200140115
007300140115       // -?DS campo TPTflo
007400140115     d dTPT01        e ds
007500140115
007600140115       // -?Costante per controllo campi numerici
007700140115     d c_Digits        c                   const('0123456789')
007800051012
007900920529     I/SPACE 3
008000920529     C*****************************************************************
008100920529     C* RIEPILOGO INDICATORI
008200920529     C*****************************************************************
008300920609     C* 06    - TIPO CONSEGNA = ISOLE MINORI
008400920624     C* 08    - ROLLUP
008500920707     C* 10    - INTERROGAZIONE
008600941114     C* 11    - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
008700941114     C* 14    - INDICA TIPO TARIFFA/TIPO PAGAMENTO A VALORE
008800941114     C* 20    - USATO PER LOKUP
008900990217     C* 24    - TIPO TARIFFE ASSICURAZIONE X CONTO
009000990217     C* 25    - TIPO TARIFFE- RCV
009100921007     C* 26    - USATO X CONDIZIONARE L'EMISSIONE DEGLI ARROTONDAMENTI
009200921007     C* 27    - USATO X CONDIZIONARE L'EMISSIONE DEL RAPPORTO P/VOL.
009300941114     C* 28    - VISUALIZZO CODICE APPLICAZIONE
009400921006     C* 30/34 - DI COMODO
009500920609     C* 35/37 - PULIZIA SUBFILE
009600941116     C* 38    - VISUALIZZO IL MINIMO  TARIFFA
009700941116     C* 39    - VISUALIZZO IL MASSIMO TARIFFA
009800980514     C* 43    - VISUALIZZO IL FLAG MANDATO ASSICURATIVO FITTIZIO
009900060802     C* 82    - VISUALIZZO data riferimento prezzo base carburante
010000120321      * 97    - Pgm richiamato x manca tariffa TNSB48
010100920529     C*****************************************************************
010200920529     C     *ENTRY        PLIST
010300920529     C                   PARM                    KPJBA
010400120321     c                   parm                    Parm48
010500120321
010600120321      /free
010700120321       //?controllo se passati tutti i parametri
010800120321         IF  %parms = 1;
010900120321           clear tnsb48ds;
011000120321           *in97 = *off;
011100120321         ELSE;
011200120321           clear tnsb48ds;
011300120321           tnsb48ds = parm48;
011400120321           *in97 = *on;
011500120321         ENDIF;
011600120321
011700120321         clear OSB48err;
011800120321         clear OSB48msg;
011900120321      /end-free
012000120321
012100920529     C                   Z-ADD     1             CODUT
012200920529     C                   CALL      'X§PARUT'
012300920529     C                   PARM                    UT§DSE
012400920529     C                   MOVEL     RAGUT         RSUT             20
012500920529     C                   MOVEL     REC80         CNCR80
012600920615     C                   MOVEL     KPJBU         PARAM5
012700080609
012800080609     c/exec sql
012900080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
013000080609     c/end-exec
013100080609
013200920529     C*---------------------------------------------------------------*
013300990622     C* ACCESSO TITPT01L/TITPD01L
013400920612     C     KTPT          KLIST
013500920612     C                   KFLD                    VIDCLI
013600920612     C                   KFLD                    VIDCTR
013700920612     C                   KFLD                    VIDPRG
013800920612     C                   KFLD                    VIDFTC
013900940713     C* ACCESSO TABEL00F
014000920608     C     KTAB          KLIST
014100920608     C                   KFLD                    CODUT
014200941114     C                   KFLD                    COD
014300941114     C                   KFLD                    KEY
014400920608     C     KTAB2         KLIST
014500920608     C                   KFLD                    CODUT
014600920608     C                   KFLD                    COD
014700941114     C***
014800941114     C** DEFINIZIONE CAMPI
014900941114     C***
015000941114     C     *LIKE         DEFINE    TBLCOD        COD
015100941114     C     *LIKE         DEFINE    TBLKEY        KEY
015200920608     C*
015300920529     C*---------------------------------------------------------------*
015400920707     C* 10 ON - INTERROGAZIONE
015500920707     C                   SETON                                        10
015600080609
015700080609      * carico tabella filiali attive al lasciato avviso
015800080609     c                   exsr      carlav
015900080609
016000051012     c                   movel     vidcli        kscfil
016100051012
016200040428      * verifico se si tratta di tariffa CARTELLO
016300040428     c                   movel     vidcli        w0005             5 0
016400040428     c                   if        w0005 = 88888
016500040428     c                   seton                                        44
016600040428     c                   else
016700040428     c                   setoff                                       44
016800040428     c                   endif
016900920529     C* CARICO SUBFILE
017000040428     C                   EXSR      CARICA
017100920609     C* SE NON CARICA NIENTE ESCO
017200941114    1C     NRR           IFEQ      0
017300920529     C                   GOTO      FINE
017400941114    1C                   END
017500920529     C*
017600920529     C     INIZIO        TAG
017700940913     C                   WRITE     TA67T01
017800940913     C                   WRITE     TA67Z03
017900940913     C                   EXFMT     TA67C02
018000920529     C*
018100921119     C** CMD3  - FINE LAVORO
018200921119     C   KC              MOVEL     'A'           FLG2
018300921119     C   KC              MOVEL     PARAM5        KPJBU
018400921119     C   KC
018500920611     C** CMD12 - FINE LAVORO
018600921119     COR KL              GOTO      FINE
018700120321
018800120321       //?F1 - dati manca tariffa
018900120321     c                   IF        *inka
019000120321     c                   exsr      gesw01
019100120321     c                   goto      inizio
019200120321     c                   ENDIF
019300920609     C*
019400030718     C** CMD07 - INTERROGAZIONE STORICO VARIAZIONI
019500030718     c                   if        *inkg
019600030718     c                   clear                   tnta04ds
019700030718     c                   eval      i04ksc = vidcli
019800030718     c                   eval      i04ctr = vidctr
019900030718     c                   eval      i04prg = vidprg
020000030718     c                   eval      i04rag = ragdes
020100030718     c                   eval      i04dct = viddcf
020200030718     c                   eval      i04dcv = viddcv
020300030718     c                   eval      i04div = vicdiv
020400030718     c                   eval      i04ndf = flgnet
020500030718      *
020600030718     c                   call      'TNTA04R'
020700030718     c                   parm                    tnta04ds
020800030718      *
020900030718     c                   goto      inizio
021000030718      *
021100030718     c                   endif
021200030718     C*
021300920529     C* CONTROLLO DATI SUBFILE
021400920529     C                   SETOFF                                       31
021500940913     C                   READC     TA67S02                                31
021600921119     C   31              GOTO      FINE
021700941114    1C     *IN31         DOWEQ     '0'
021800921006     C*
021900941114    2C     VIDSCE        IFEQ      '1'
022000941114    3C     VIDFLG        IFEQ      'S'
022100921006     C                   MOVEL     VIDFTC        VI2FTC
022200921006     C                   MOVEL     VIDDES        VI2DTC
022300980317     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
022400980317     C                   MOVEL     *BLANKS       TTR
022500980317     C                   MOVEL     *BLANKS       DTR
022600980317     C                   MOVEL     *BLANKS       TTP
022700980317     C                   MOVEL     *BLANKS       WTT
022800980520     C                   MOVEL     *BLANKS       ARR
022900980520     C                   MOVEL     *BLANKS       RPV
023000980317     C*
023100921006     C*
023200941114     C* CARICO I TIPI TARIFFA VALIDI PER LA CONS.PARTICOL. SELEZIONATA
023300941114     C                   MOVEL     '1P'          COD
023400941114     C                   MOVEL     *BLANKS       KEY
023500941114     C                   MOVEL     VIDFTC        KEY
023600941114     C     KTAB          CHAIN     TABEL                              34
023700941114    4C     *IN34         IFEQ      *OFF
023800941114     C                   MOVEL     TBLUNI        DS1P
023900980317     C* CARICO I TIPI TARIFFE ABILITATI A QUESTO TARIFFA PARTICOLARE
024000980317     C                   Z-ADD     0             X                 3 0
024100980504     C                   MOVEL     §1PTT0        WTT(1)
024200980504     C                   MOVEL     §1PTT1        WTT(2)
024300980504     C                   MOVEL     §1PTT2        WTT(3)
024400980504     C                   MOVEL     §1PTT3        WTT(4)
024500980504     C                   MOVEL     §1PTT4        WTT(5)
024600980504     C                   MOVEL     §1PTT5        WTT(6)
024700980504     C                   MOVEL     §1PTT6        WTT(7)
024800980504     C                   MOVEL     §1PTT7        WTT(8)
024900980504     C                   MOVEL     §1PTT8        WTT(9)
025000980504     C                   MOVEL     §1PTT9        WTT(10)
025100980504     C                   MOVEL     §1PTTV        WTT(11)
025200980504     C*
025300980317     C* CARICO TABELLA CODICI TARIFFA
025400980317     C                   EXSR      CARTAB
025500941114     C* 11 ON  - VISUALIZZO IL VALORE MERCE E IL FLAG VALORE MERCE
025600941114     C     §1PVVM        COMP      'S'                                    11
025700941114     C* 28 ON  - VISUALIZZO IL TIPO APPLICAZIONE
025800941114     C     §1PVCA        COMP      'S'                                    28
025900941116     C* 38 ON  - VISUALIZZO IL MINIMO  TARIFFA
026000941116     C     §1PVMN        COMP      'S'                                    38
026100941116     C* 39 ON  - VISUALIZZO IL MASSIMO TARIFFA
026200941116     C     §1PVMX        COMP      'S'                                    39
026300980514     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO FITTIZIO
026400990622     C     VIDFTC        COMP      'C '                                   43
026500060802     C* 82 ON  - VISUALIZZO DATA riferimento prezzo base carburante
026600060802     C     §1PDRF        COMP      'S'                                    82
026700941114    4C                   ENDIF
026800941114     C*
026900941114     C                   EXSR      VIDEO2
027000941114    3C                   END
027100920708     C*
027200940913     C     NRR           CHAIN     TA67S02                            31
027300920529     C                   MOVEL     ' '           VIDSCE
027400940913     C                   UPDATE    TA67S02
027500941114    2C                   END
027600921006     C*
027700940913     C                   READC     TA67S02                                31
027800941114    1C                   END
027900921119     C                   GOTO      INIZIO
028000920529     C*
028100920529     C     FINE          TAG
028200120321
028300120321     c                   IF        *in97
028400120321     c                   eval      parm48 = tnsb48ds
028500120321     c                   ENDIF
028600120321
028700920529     C                   SETON                                        LR
028800920609     C*
028900920609     C*--- CARICO TABELLA CODICI TARIFFA -----------------------------*
029000920609     C     CARTAB        BEGSR
029100941114     C****  CODICI TARIFFA E DESCRIZIONI  ****
029200920609     C                   MOVEL     'TR'          COD
029300920609     C                   Z-ADD     1             X                 3 0
029400980317     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
029500980317     C* PARTICOLARE
029600980317    1C     WTT(X)        DOWNE     ' '
029700980317     C                   MOVEL     WTT(X)        KEY
029800980317     C     KTAB          CHAIN     TABEL                              30
029900980317    2C     *IN30         IFEQ      '0'
030000980317     C     TBLFLG        ANDEQ     *BLANKS
030100940921     C                   MOVEL     TBLUNI        DSTR
030200940913     C*
030300980317     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
030400980317    3C     §TRUPA        IFEQ      'S'
030500920612     C                   MOVEL     TBLKEY        TTR(X)
030600941114     C                   MOVEL     §TRDES        DTR(X)
030700941114     C* §TRTAP = "S" --> TARIFFA IN %    = " " --> TARIFFA IN £
030800941114     C                   MOVEL     §TRTAP        TTP(X)
030900980520     C* ARROTONDAMENTI
031000980520     C                   MOVEL     §TRARR        ARR(X)
031100980520     C* RAPPORTO PESO VOLUME
031200980520     C                   MOVEL     §TRRPV        RPV(X)
031300940913     C                   END
031400940913     C*
031500920609     C                   END
031600980317     C*
031700980317     C                   ADD       1             X
031800980317     C                   ENDDO
031900921006     C*
032000920609     C                   ENDSR
032100080609
032200080609      *------------------------------------------------------------------------*
032300080609      * Carico filiali abilitate al lasciato avviso
032400080609      *------------------------------------------------------------------------*
032500080609     c     carlav        begsr
032600080609
032700080609     c                   clear                   sklav
032800080609     c                   clear                   xx
032900080609
033000080609     c/exec sql
033100080609     c+ declare lav cursor for select tntbe01l.*
033200080609     c+ from tntbe01l
033300080609     c+ where tbecod='LAV' and tbeatb = ''
033400080609     c/end-exec
033500080609
033600080609     c/exec sql
033700080609     c+ open lav
033800080609     c/end-exec
033900080609
034000080609     c                   do        *hival
034100080609
034200080609     c/exec sql
034300080609     c+ fetch next from lav into :tntbeds
034400080609     c/end-exec
034500080609
034600080609     c                   select
034700080609     c                   when      sqlcod = 100
034800080609     c                   leave
034900080609     c                   when      sqlcod < 0
035000080609     c                   other
035100080609     c                   eval      dlav = tbeuni
035200080609      * carico la schiera della filiali abilitate
035300080616     c                   if        d§lavtar > *date
035400080609     c                   iter
035500080609     c                   endif
035600080609     c                   eval      xx = xx + 1
035700080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
035800080609
035900080609     c                   endsl
036000080609
036100080609     c                   enddo
036200080609
036300080609     c/exec sql
036400080609     c+ close lav
036500080609     c/end-exec
036600080609
036700080609
036800080609     c                   endsr
036900920609     C*
037000920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
037100920529     C     CARICA        BEGSR
037200920529     C                   SETOFF                                       3031
037300920529     C* PULIZIA SUBFILE
037400920529     C                   EXSR      PULSF
037500920529     C*
037600920529     C                   Z-ADD     0             NRR               4 0
037700920529     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
037800920529     C                   MOVEL     '1P'          COD
037900920608     C     KTAB2         SETLL     TABEL
038000920608     C     KTAB2         READE     TABEL                                  30
038100920611     C     *IN30         DOWEQ     '0'
038200920611     C* CONTROLLO VALIDITA' CODICE CONSEGNA
038300920611     C     TBLFLG        IFEQ      *BLANKS
038400920529     C                   MOVEL     TBLUNI        DS1P
038500020204      * FLAG DI COMODO PER TABELLA 'SP'
038600020204     C                   CLEAR                   FLAGSP
038700020201      *
038800080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
038900080609      * per le filiali non abilitate, accetto cartello
039000051012     c                   If        tblkey = 'c       '  and
039100080609     c                             kscfil <> 888
039200080609     c                   if        %lookup(kscfil:sklav) = *zeros
039300080609     c                             and %lookup(999:sklav) = *zeros
039400051012     c                   goto      legtab
039500080609     c                   endif
039600051012     c                   endif
039700060308      * ESCLUDO la tariffa particolare "d" se tariffa di cartello
039800060308     c                   if        tblkey = 'd       '  and *in44
039900060308     c                   goto      legtab
040000060308     c                   endif
040100051012      *
040200020201      *----------------------
040300020201      * NUOVI CONTROLLI!!!!!!
040400020201      *----------------------
040500020204      * Carico solo le tariffe particolari che hanno flag <> 'N'
040600020201      * Per tariffa con tipo servizio poste controllo flag §1pfpt
040700020201     C     PARTSP        IFEQ      'P'
040800020201     C     §1PFPT        ANDEQ     'N'
040900020201     C                   GOTO      LEGTAB
041000020201     C                   ENDIF
041100020201      * Per tariffa con Network DPD controllo flag §1pfdp
041200020201     C     FLGNET        IFEQ      'D'
041300020201     C     §1PFDP        ANDEQ     'N'
041400020201     C                   GOTO      LEGTAB
041500020201     C                   ENDIF
041600020201      * Per tariffa con Network FedEx controllo flag §1pffe
041700020201     C     FLGNET        IFEQ      'F'
041800020201     C     §1PFFE        ANDEQ     'N'
041900040428      * tariffa FEDEX abilitata solo per le tariffe di cartello e 44 spento
042000040428      * (44 acceso solo se sto modificando una tariffa di cartello)
042100040428     c     flgnet        oreq      'F'
042200040428     c     §1pffe        andeq     'C'
042300040428     c     *in44         andeq     *off
042400020201     C                   GOTO      LEGTAB
042500020201     C                   ENDIF
042600020201      * Per tariffa Italia controllo flag §1ptco
042700020201     C     PARFIE        IFEQ      'I'
042800020205     C     PARTSP        ANDNE     'P'
042900020201     C     §1PTCO        ANDEQ     'N'
043000020201     C                   GOTO      LEGTAB
043100020201     C                   ENDIF
043200020201      * Per tariffa Estero controllo flag §1pfie
043300020201     C     PARFIE        IFEQ      'E'
043400020205     C     FLGNET        ANDEQ     ' '
043500020201     C     §1PFIE        ANDEQ     'N'
043600020201     C                   GOTO      LEGTAB
043700020201     C                   ENDIF
043800020225      * Per tariffe di cartello verifico che non sia tipo servizio "P"
043900020225     C     PARFIE        IFEQ      ' '
044000020225     C     PARTSP        ANDNE     'P'
044100020225     C     §1PTCO        ANDEQ     'N'
044200020225     C                   GOTO      LEGTAB
044300020225     C                   ENDIF
044400020201      *
044500941025     C*
044600941025     C* CONTROLLO SE SI TRATTA DI CONS.PARTIC. UTILIZZABILE IN TARIFFA
044700921006     C     §1PUTA        IFEQ      'S'
044800970325     C*
044900921006     C                   MOVEL     TBLKEY        VIDFTC
045000920529     C                   MOVEL     §1PDES        VIDDES
045100020201      *
045200020201      * Per tariffa FedEx controllo se devo recupare la descrizione
045300020201      * dalla tabella SP
045400020201     C     FLGNET        IFEQ      'F'
045500020201     C     §1PDFE        ANDEQ     'S'
045600020204     C                   MOVEL     '1'           FLAGSP            1
045700020201     C                   CLEAR                   DSSP
045800020201     C                   MOVEL     'SP'          COD
045900020201     C                   MOVEL     *BLANKS       KEY
046000020201     C                   MOVEL     VIDFTC        KEY
046100020201     C     KTAB          CHAIN     TABEL                              34
046200020201     C     *IN34         IFEQ      *OFF
046300020201     C                   MOVEL     TBLUNI        DSSP
046400020201     C                   ENDIF
046500020201     C     §SPDFE        IFNE      *BLANKS
046600020201     C                   MOVEL     §SPDFE        VIDDES
046700020201     C                   ENDIF
046800020201     C                   ENDIF
046900020201      *
047000920529     C* CAMPO SELEZIONE
047100920529     C                   MOVEL     ' '           VIDSCE
047200920529     C*
047300990622     C     KTPT          CHAIN     TITPT000                           31
047400990907     C                   Z-ADD     0             VIDDUV
047500920529     C     *IN31         IFEQ      '0'
047600920529     C* CONTROLLO VALIDITA' TARIFFA
047700920529     C     TPTATB        IFEQ      ' '
047800920529     C* TARIFFA PARTICOLARE GIA' ESISTENTE
047900920529     C                   MOVEL     'S'           VIDFLG
048000990907     C* VALORIZZO L'ULTIMA DATA VARIAZIONE
048100990907     C*
048200990907     C* GIRO DATA
048300990907     C*
048400990907     C                   Z-ADD     TPTDUV        G02INV
048500990907     C                   MOVEL     '3'           G02ERR
048600990907     C                   CALL      'XSRDA8'
048700990907     C                   PARM                    WLBDAT
048800990907     C*
048900990907     C                   Z-ADD     G02DAT        VIDDUV
049000990907     C*
049100920529     C                   ELSE
049200920529     C* TARIFFA ANNULLATA
049300920707     C                   MOVEL     ' '           VIDFLG
049400920529     C                   END
049500920529     C*
049600920529     C                   ELSE
049700920529     C* TARIFFA PARTICOLARE NON ESISTENTE
049800920707     C                   MOVEL     ' '           VIDFLG
049900920529     C                   END
050000920529     C*
050100990217     C* SE C'E' LA TARIFFA -->VISUALIZZO
050200130607    4C**** VIDFLG        IFEQ      'S'
050300920529     C                   ADD       1             NRR
050400940913     C                   WRITE     TA67S02
050500130607    4C****               END
050600990217    3C                   END
050700990217    2C                   END
050800920529     C*
050900000420     C     LEGTAB        TAG
051000020204     C     FLAGSP        IFNE      *BLANKS
051100020201     C                   MOVEL     '1P'          COD
051200020204     C     KTAB          SETGT     TABEL
051300020204     C                   ENDIF
051400920611     C     KTAB2         READE     TABEL                                  30
051500990217    1C                   END
051600920529     C                   Z-ADD     1             REC
051700920529     C                   ENDSR
051800920529     C*
051900920529     C*--- PULIZIA SUBFILE -------------------------------------------*
052000920529     C     PULSF         BEGSR
052100920529     C                   SETON                                        35
052200940913     C                   WRITE     TA67C02
052300920529     C                   SETOFF                                       35
052400920529     C                   ENDSR
052500920529     C*
052600920529     C*--- SR SECONDA VIDEATA ----------------------------------------*
052700920529     C     VIDEO2        BEGSR
052800941116     C                   SETOFF                                       0614
052900921006     C                   SETOFF                                       2425
053000920709     C* 37 - LO UTILIZZO PER EMETTERE IL SUBFILE VUOTO QUANDO C'E'
053100920709     C*      LA TESTATA MA NON CI SONO RIGHE DI DETTAGLIO
053200920709     C                   SETOFF                                       37
053300941114     C*
053400990217     C* 24 ON - TIPO CONSEGNA  ASSICURAZIONE X CONTO
053500990217     C**         VI2FTC    COMP 'C'                      24
053600990217     C     §1PASC        IFEQ      'S'
053700990217     C                   SETON                                            24
053800990217     C                   ENDIF
053900990217     C* 25 ON - TIPO CONSEGNA RCV
054000990217     C****       VI2FTC    COMP 'R'                      25
054100990217     C     §1PRCV        IFEQ      'S'
054200990217     C                   SETON                                            25
054300990217     C                   ENDIF
054400941114     C*
054500920611     C* PULIZIA SUBFILE
054600920611     C                   EXSR      PULSF2
054700941114     C*
054800941114     C* 06 ON - TIPO CONSEGNA = I-ISOLE MINORI
054900061220     C***  VI2FTC        COMP      'I '                                   06
055000061220     C* 06 ON - APPLICAZIONE CON INOLTRO  PER ISOLE/LOC.DISAGIATE/C.STO
055100061220     C     §1PAIN        COMP      'S'                                    06
055200921006     C* CARICO DATI SFLCONTROL
055300921006     C                   EXSR      CARCTL
055400920529     C* CARICO SUBFILE
055500941114     C                   EXSR      CARSFL
055600920617     C*
055700920617     C                   Z-ADD     1             REC2
055800940913     C                   WRITE     TA67T01
055900940913     C                   WRITE     TA67Z05
056000941114     C                   WRITE     TA67D06
056100941114     C                   EXFMT     TA67C07
056200920611     C                   ENDSR
056300920609     C*
056400920611     C*--- PULIZIA SUBFILE -------------------------------------------*
056500920611     C     PULSF2        BEGSR
056600920611     C                   SETON                                        37
056700941114     C                   WRITE     TA67C07
056800920611     C                   SETOFF                                       37
056900920611     C                   ENDSR
057000920611     C*
057100920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
057200941114     C     CARSFL        BEGSR
057300920707     C                   SETOFF                                       3233
057400920616     C*
057500920611     C                   MOVEL     'CT'          COD
057600920611     C                   MOVEL     *BLANKS       KEY
057700920529     C* RICERCA
057800990622     C     KTPT          SETLL     TITPD000
057900920608     C                   Z-ADD     0             NR2               4 0
058000990622     C     KTPT          READE     TITPD000                               32
058100920529     C* 32 ON - FINE LETTURA
058200920611     C     *IN32         DOWEQ     '0'
058300920611     C* CONTROLLO VALIDITA' TARIFFA
058400920611     C     TPDATB        IFEQ      ' '
058500920529     C* CODICE TASSAZIONE
058600920529     C                   MOVEL     TPDCTS        VI2CTS
058700920608     C* DECODIFICA CODICE TASSAZIONE
058800920609     C                   MOVEL     VI2CTS        KEY
058900920608     C     KTAB          CHAIN     TABEL                              33
059000920612     C  N33              DO
059100920612     C                   MOVEL     TBLUNI        DSCT
059200920612     C                   MOVEL     §CTDES        VI2DCT
059300920612     C                   MOVEL     §CTCOR        VI2ORP
059400920612     C                   END
059500920608     C   33              MOVEL     *BLANKS       VI2DCT
059600920615     C* SCAGLIONE
059700940713     C                   MOVEL     TPDSGL        VI2SGL
059800920608     C* IMPORTO TARIFFA
059900941114     C                   Z-ADD     TPDITR        VI2ITR
060000940713     C* MINIMO  TARIFFA
060100940713     C                   MOVEL     TPDMIN        VI2MIN
060200940713     C* MASSIMO TARIFFA
060300940713     C                   MOVEL     TPDMAX        VI2MAX
060400920611     C* 06 ON - TIPO CONSEGNA = I  CARICO FLAG TARIFFA PROVINCIA
060500920611     C   06              MOVEL     TPDAIN        VI2AIN
060600920610     C*
060700920608     C* FLAG ANNULLAMENTO
060800920610     C                   MOVEL     ' '           VI2ATB
060900920610     C*
061000920608     C                   ADD       1             NR2
061100920608     C*
061200941114     C                   WRITE     TA67S07
061300920611     C                   END
061400920608     C*
061500990622     C     KTPT          READE     TITPD000                               32
061600920611     C                   END
061700920611     C*
061800920709     C* SE NON CI SONO RIGHE DI DETTAGLIO EMETTO LA TESTATA
061900920709     C     NR2           IFEQ      0
062000920709     C* 37 - LO UTILIZZO PER EMETTERE IL SUBFILE VUOTO QUANDO C'E'
062100920709     C*      LA TESTATA MA NON CI SONO RIGHE DI DETTAGLIO
062200920709     C                   SETON                                        37
062300920709     C                   END
062400920709     C*
062500920608     C                   ENDSR
062600920608     C*
062700920609     C*--- CARICO DATI SUBFILE CONTROL -------------------------------*
062800920608     C     CARCTL        BEGSR
062900921006     C                   SETOFF                                       2627
063000050927     C                   CLEAR                   TPG1
063100050927     C                   CLEAR                   DPG1
063200050927     C                   CLEAR                   TPG2
063300050927     C                   CLEAR                   DPG2
063400050927     C                   CLEAR                   TPG3
063500050927     C                   CLEAR                   DPG3
063600050927     C                   CLEAR                   TPG4
063700050927     C                   CLEAR                   DPG4
063800050927     C                   CLEAR                   TPG5
063900050927     C                   CLEAR                   DPG5
064000050927     C                   CLEAR                   TPG6
064100050927     C                   CLEAR                   DPG6
064200050927     C                   CLEAR                   TPG7
064300050927     C                   CLEAR                   DPG7
064400050927     C                   CLEAR                   TPG8
064500050927     C                   CLEAR                   DPG8
064600941114     C*
064700941114     C****  CARICO LE DECODIFICHE DEI TIPI TARIFFA  ****
064800941114     C* CARICO I TIPI TARIFFA PRENDENDOLI DALLA TABELLA "TR"
064900941114     C     TTR(1)        IFNE      ' '
065000941114     C                   MOVEL     TTR(1)        TPG1
065100941114     C                   MOVEL     DTR(1)        DPG1
065200941114     C                   END
065300941114     C     TTR(2)        IFNE      ' '
065400941114     C                   MOVEL     TTR(2)        TPG2
065500941114     C                   MOVEL     DTR(2)        DPG2
065600941114     C                   END
065700941114     C     TTR(3)        IFNE      ' '
065800941114     C                   MOVEL     TTR(3)        TPG3
065900941114     C                   MOVEL     DTR(3)        DPG3
066000941114     C                   END
066100941114     C     TTR(4)        IFNE      ' '
066200941114     C                   MOVEL     TTR(4)        TPG4
066300941114     C                   MOVEL     DTR(4)        DPG4
066400941114     C                   END
066500941114     C     TTR(5)        IFNE      ' '
066600941114     C                   MOVEL     TTR(5)        TPG5
066700941114     C                   MOVEL     DTR(5)        DPG5
066800941114     C                   END
066900941114     C     TTR(6)        IFNE      ' '
067000941114     C                   MOVEL     TTR(6)        TPG6
067100941114     C                   MOVEL     DTR(6)        DPG6
067200941114     C                   END
067300980504     C     TTR(7)        IFNE      ' '
067400980504     C                   MOVEL     TTR(7)        TPG7
067500980504     C                   MOVEL     DTR(7)        DPG7
067600980504     C                   END
067700980504     C     TTR(8)        IFNE      ' '
067800980504     C                   MOVEL     TTR(8)        TPG8
067900980504     C                   MOVEL     DTR(8)        DPG8
068000980504     C                   END
068100941114     C*
068200941114     C* CARICO DATI SFLCONTROL
068300990622     C     KTPT          CHAIN     TITPT000                           33
068400941114    1C     *IN33         IFEQ      '0'
068500140115     c                   eval      dTPT01 = TPTflo
068600921007     C*
068700921007     C* TIPO TARIFFA/TIPO PAGAMENTO
068800921007     C                   MOVEL     TPTTPG        VI2TPG
068900980520     C* CONTROLLO IL TIPO TARIFFA   NELLA TABELLA "TR"
069000941114     C                   Z-ADD     1             X
069100941114     C     VI2TPG        LOOKUP    TTR(X)                                 20
069200941114     C* CONTROLLO SE TARIFFA IN % O IN £
069300941114     C   20TTP(X)        COMP      'S'                                    14
069400980520     C* ARROTONDAMENTI
069500980520     C   20              MOVEL     ARR(X)        WARR              1
069600980520     C* RAPPORTO PESO VOLUME
069700980520     C   20              MOVEL     RPV(X)        WRPV              1
069800941114     C*
069900980520     C* CONDIZIONO L'EMISSIONE DEGLI ARROTOND. E DEL RAPPORTO PESO/VOL.
070000980520     C     WRPV          IFEQ      'S'
070100921006     C                   SETON                                        27
070200980520     C                   END
070300980520     C     WARR          IFEQ      'S'
070400921006     C                   SETON                                        26
070500980520     C                   END
070600941114     C* 26 ON  - VISUALIZZO  ARROTONDAMENTI
070700941114    2C     *IN26         IFEQ      *ON
070800920608     C                   MOVEL     TPTARF        VI2ARF
070900940713     C                   MOVEL     TPTARL        VI2ARL
071000920608     C                   MOVEL     TPTARO        VI2ARO
071100941114    2C                   END
071200941114     C* 27 ON  - VISUALIZZO RAPPORTO PESO/VOLUME
071300921006     C   27              Z-ADD     TPTRPV        VI2RPV
071400921006     C*
071500941114     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
071600941114    2C     *IN11         IFEQ      *ON
071700921006     C* VALORE MERCE/LIMITE RISARCIBILE
071800921006     C                   Z-ADD     TPTVLM        VI2VLM
071900920624     C* TIPO VALORE
072000940713     C                   MOVEL     TPTFVM        VI2FVM
072100921006     C*
072200980317     C* DECODIFICA TIPO VALORE MERCE DALLA TABELLA TR
072300921006     C                   MOVEL     *BLANKS       DESFVM
072400921006     C*
072500980317     C                   MOVEL     'TR'          COD
072600921006     C                   MOVEL     *BLANKS       KEY
072700921006     C                   MOVEL     VI2FVM        KEY
072800921006     C     KTAB          CHAIN     TABEL                              34
072900941114    3C     *IN34         IFEQ      '0'
073000980317     C                   MOVEL     TBLUNI        DSTR
073100980317     C     §TRUCV        IFEQ      'S'
073200980317     C                   MOVEL     §TRDES        DESFVM
073300921006     C                   END
073400941114    3C                   END
073500941114    2C                   END
073600921006     C*
073700941114     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
073800941114     C   28              MOVEL     TPTAPL        VI2APL
073900990217     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO
074000980504     C                   MOVE      *BLANKS       VI2MAS
074100990907     C   43              MOVEL     TPTTMA        VI2MAS
074200941114    1C                   END
074300060802      * 82 ON  - Visualizzo la data riferimento prezzo base del carburante
074400060802     c                   If        *in82
075000140224     c                   z-add     TPTDPB        vO2dpb
075100140115     c                   IF        %check(c_Digits:%subst(TPTflo:1:
075200140115     c                             %len(§TPTpma))) = *zeros
075300140115     c                   eval      VI2pma = §TPTpma
075400140115     c                   ENDIF
075500131216      * recupero il prezzo del gasolio corrispondente
075600131216     c     tptdpb        setll     tipmg01l
075700131216     c                   read      tipmg01l
075800131216     c                   if        %found(tipmg01l)
075900140224     c                   eval      vI2pmg = pmgpmg
075901140224     c                   eval      vO2sca = pmgsca
076000131216     c                   endif
076100131216
076200060802     c                   endif
076300060802
076400920608     C                   ENDSR
076500120321
076600120321      /free
076700120321       //--------------------------------------------------------------
076800120321       //?Visualizza Dati manca tariffa-
076900120321       //--------------------------------------------------------------
077000120321       BEGSR GesW01;
077100120321
077200120321         wFine = *off;
077300120321         W01dsp = ISB48dsp;
077400120321         W01tbl = ISB48tbl;
077500120321         W01lnp = ISB48lnp;
077600120321         W01lna = ISB48lna;
077700120321         W01pro = ISB48pro;
077800120321         W01cts = ISB48cts;
077900120321         W01ctr = ISB48ctr;
078000120321         W01ncl = ISB48ncl;
078100120321         W01pkf = ISB48pkf;
078200120321         W01vlf = ISB48vlf;
078300120321         W01err = ISB48err;
078400120321
078500120321         DOW wFine = *off;
078600120321           exfmt  TA67W01;
078700120321           //?- F12 = Ritorno
078800120321           IF  *inkl;
078900120321             wFine = *on;
079000120321           ENDIF;
079100120321         ENDDO;
079200120321
079300120321       ENDSR;
079400120321      /end-free
