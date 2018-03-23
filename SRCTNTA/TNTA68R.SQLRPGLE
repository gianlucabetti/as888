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
003600150219     ***D* DATA DECORRENZA TARIFFA
003700150219     ***D  PARDDT               132    139  0
003800020201     D* TIPO SEWRVIZIO BOLLA
003900020201     D  PARTSP               140    140
004000020201     D* Flag Network
004100020201     D  FLGNET               141    141
004200150219
004300990907     D WLBDAT          DS
004400990907     D  G02DAT                 1      8  0
004500990907     D  G02INV                 9     16  0
004600990907     D  G02ERR                17     17
004700990907     D  G02TGI                18     22  0
004800150219
004900920612     D KPJBA         E DS
005000920529     D DS1P          E DS
005100020201     D DSSP          E DS
005200920612     D DSCT          E DS
005300940913     D DSTR          E DS
005400150217     ***D CNCR80        E DS
005500150217     ***D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005600150217     d Azuteds       e ds                  extname(Azute00f)
005700150217     d dDatiute      e ds
005800150217     d Tibs34ds      e ds
005900150217
006000990217     D DSBS02        E DS                  EXTNAME(TIBS02DS)
006100150217
006200030718     D TNTA04DS      E DS
006300051012
006400051012     d Kscfil          s              3  0
006500080609      * schiera filiali abilitate al lasciato avviso
006600080609     d sklav           s              3  0 dim(999)
006700080609
006800080609     d xx              s              3  0
006900120321
007000120321       // -?Flag booleani
007100120321     d wFine           s               n   inz(*off)
007200080609
007300080609     d dlav          e ds
007400080609     d tntbeds       e ds                  extname(tntbe00f)
007500120321
007600120321       // -?ds dati passati da TNSB48R - Manca Tariffa
007700120321     d TNSB48ds      e ds
007800120321     d Parm48          s                   like(tnsb48ds)
007900140115
008000140115       // -?DS campo TPTflo
008100140115     d dTPT01        e ds
008200140115
008300140115       // -?Costante per controllo campi numerici
008400140115     d c_Digits        c                   const('0123456789')
008500051012
008600920529     I/SPACE 3
008700920529     C*****************************************************************
008800920529     C* RIEPILOGO INDICATORI
008900920529     C*****************************************************************
009000920609     C* 06    - TIPO CONSEGNA = ISOLE MINORI
009100920624     C* 08    - ROLLUP
009200920707     C* 10    - INTERROGAZIONE
009300941114     C* 11    - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
009400941114     C* 14    - INDICA TIPO TARIFFA/TIPO PAGAMENTO A VALORE
009500941114     C* 20    - USATO PER LOKUP
009600990217     C* 24    - TIPO TARIFFE ASSICURAZIONE X CONTO
009700990217     C* 25    - TIPO TARIFFE- RCV
009800921007     C* 26    - USATO X CONDIZIONARE L'EMISSIONE DEGLI ARROTONDAMENTI
009900921007     C* 27    - USATO X CONDIZIONARE L'EMISSIONE DEL RAPPORTO P/VOL.
010000941114     C* 28    - VISUALIZZO CODICE APPLICAZIONE
010100921006     C* 30/34 - DI COMODO
010200920609     C* 35/37 - PULIZIA SUBFILE
010300941116     C* 38    - VISUALIZZO IL MINIMO  TARIFFA
010400941116     C* 39    - VISUALIZZO IL MASSIMO TARIFFA
010500980514     C* 43    - VISUALIZZO IL FLAG MANDATO ASSICURATIVO FITTIZIO
010600060802     C* 82    - VISUALIZZO data riferimento prezzo base carburante
010700150219      * 90    - Visualizzo "Applicazione VMA (N=No)"
010800120321      * 97    - Pgm richiamato x manca tariffa TNSB48
010900920529     C*****************************************************************
011000920529     C     *ENTRY        PLIST
011100920529     C                   PARM                    KPJBA
011200120321     c                   parm                    Parm48
011300120321
011400120321      /free
011500120321       //?controllo se passati tutti i parametri
011600120321         IF  %parms = 1;
011700120321           clear tnsb48ds;
011800120321           *in97 = *off;
011900120321         ELSE;
012000120321           clear tnsb48ds;
012100120321           tnsb48ds = parm48;
012200120321           *in97 = *on;
012300120321         ENDIF;
012400120321
012500120321         clear OSB48err;
012600120321         clear OSB48msg;
012700120321      /end-free
012800120321
012900150217     C                   Z-ADD     1             CODUT             1 0
013000150217     ***C                   CALL      'X§PARUT'
013100150217     ***C                   PARM                    UT§DSE
013200150217     ***C                   MOVEL     RAGUT         RSUT             20
013300150217     ***C                   MOVEL     REC80         CNCR80
013400920615     C                   MOVEL     KPJBU         PARAM5
013500150217
013600150217     c     *dtaara       define    §azute        azuteds
013700150217     c     *dtaara       define    §datiute      ddatiute
013800150217     c                   in(E)     *dtaara
013900150217     c                   If        %error  or RSUT = *blanks
014000150217     c                   Clear                   Tibs34ds
014100150217     c                   Call      'TIBS34R'
014200150217     c                   Parm                    Tibs34ds
014300150217     c                   In        *dtaara
014400150217     c                   EndIf
014500080609
014600080609     c/exec sql
014700080609     c+ set option dynusrprf = *owner, closqlcsr = *endmod
014800080609     c/end-exec
014900080609
015000920529     C*---------------------------------------------------------------*
015100990622     C* ACCESSO TITPT01L/TITPD01L
015200920612     C     KTPT          KLIST
015300920612     C                   KFLD                    VIDCLI
015400920612     C                   KFLD                    VIDCTR
015500920612     C                   KFLD                    VIDPRG
015600920612     C                   KFLD                    VIDFTC
015700940713     C* ACCESSO TABEL00F
015800920608     C     KTAB          KLIST
015900920608     C                   KFLD                    CODUT
016000941114     C                   KFLD                    COD
016100941114     C                   KFLD                    KEY
016200920608     C     KTAB2         KLIST
016300920608     C                   KFLD                    CODUT
016400920608     C                   KFLD                    COD
016500941114     C***
016600941114     C** DEFINIZIONE CAMPI
016700941114     C***
016800941114     C     *LIKE         DEFINE    TBLCOD        COD
016900941114     C     *LIKE         DEFINE    TBLKEY        KEY
017000920608     C*
017100920529     C*---------------------------------------------------------------*
017200920707     C* 10 ON - INTERROGAZIONE
017300920707     C                   SETON                                        10
017400080609
017500080609      * carico tabella filiali attive al lasciato avviso
017600080609     c                   exsr      carlav
017700080609
017800051012     c                   movel     vidcli        kscfil
017900051012
018000040428      * verifico se si tratta di tariffa CARTELLO
018100040428     c                   movel     vidcli        w0005             5 0
018200040428     c                   if        w0005 = 88888
018300040428     c                   seton                                        44
018400040428     c                   else
018500040428     c                   setoff                                       44
018600040428     c                   endif
018700920529     C* CARICO SUBFILE
018800040428     C                   EXSR      CARICA
018900920609     C* SE NON CARICA NIENTE ESCO
019000941114    1C     NRR           IFEQ      0
019100920529     C                   GOTO      FINE
019200941114    1C                   END
019300920529     C*
019400920529     C     INIZIO        TAG
019500940913     C                   WRITE     TA67T01
019600940913     C                   WRITE     TA67Z03
019700940913     C                   EXFMT     TA67C02
019800920529     C*
019900921119     C** CMD3  - FINE LAVORO
020000921119     C   KC              MOVEL     'A'           FLG2
020100921119     C   KC              MOVEL     PARAM5        KPJBU
020200921119     C   KC
020300920611     C** CMD12 - FINE LAVORO
020400921119     COR KL              GOTO      FINE
020500120321
020600120321       //?F1 - dati manca tariffa
020700120321     c                   IF        *inka
020800120321     c                   exsr      gesw01
020900120321     c                   goto      inizio
021000120321     c                   ENDIF
021100920609     C*
021200030718     C** CMD07 - INTERROGAZIONE STORICO VARIAZIONI
021300030718     c                   if        *inkg
021400030718     c                   clear                   tnta04ds
021500030718     c                   eval      i04ksc = vidcli
021600030718     c                   eval      i04ctr = vidctr
021700030718     c                   eval      i04prg = vidprg
021800030718     c                   eval      i04rag = ragdes
021900030718     c                   eval      i04dct = viddcf
022000030718     c                   eval      i04dcv = viddcv
022100030718     c                   eval      i04div = vicdiv
022200030718     c                   eval      i04ndf = flgnet
022300030718      *
022400030718     c                   call      'TNTA04R'
022500030718     c                   parm                    tnta04ds
022600030718      *
022700030718     c                   goto      inizio
022800030718      *
022900030718     c                   endif
023000030718     C*
023100920529     C* CONTROLLO DATI SUBFILE
023200920529     C                   SETOFF                                       31
023300940913     C                   READC     TA67S02                                31
023400921119     C   31              GOTO      FINE
023500941114    1C     *IN31         DOWEQ     '0'
023600921006     C*
023700941114    2C     VIDSCE        IFEQ      '1'
023800941114    3C     VIDFLG        IFEQ      'S'
023900921006     C                   MOVEL     VIDFTC        VI2FTC
024000921006     C                   MOVEL     VIDDES        VI2DTC
024100980317     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
024200980317     C                   MOVEL     *BLANKS       TTR
024300980317     C                   MOVEL     *BLANKS       DTR
024400980317     C                   MOVEL     *BLANKS       TTP
024500980317     C                   MOVEL     *BLANKS       WTT
024600980520     C                   MOVEL     *BLANKS       ARR
024700980520     C                   MOVEL     *BLANKS       RPV
024800980317     C*
024900921006     C*
025000941114     C* CARICO I TIPI TARIFFA VALIDI PER LA CONS.PARTICOL. SELEZIONATA
025100941114     C                   MOVEL     '1P'          COD
025200941114     C                   MOVEL     *BLANKS       KEY
025300941114     C                   MOVEL     VIDFTC        KEY
025400941114     C     KTAB          CHAIN     TABEL                              34
025500941114    4C     *IN34         IFEQ      *OFF
025600941114     C                   MOVEL     TBLUNI        DS1P
025700980317     C* CARICO I TIPI TARIFFE ABILITATI A QUESTO TARIFFA PARTICOLARE
025800980317     C                   Z-ADD     0             X                 3 0
025900980504     C                   MOVEL     §1PTT0        WTT(1)
026000980504     C                   MOVEL     §1PTT1        WTT(2)
026100980504     C                   MOVEL     §1PTT2        WTT(3)
026200980504     C                   MOVEL     §1PTT3        WTT(4)
026300980504     C                   MOVEL     §1PTT4        WTT(5)
026400980504     C                   MOVEL     §1PTT5        WTT(6)
026500980504     C                   MOVEL     §1PTT6        WTT(7)
026600980504     C                   MOVEL     §1PTT7        WTT(8)
026700980504     C                   MOVEL     §1PTT8        WTT(9)
026800980504     C                   MOVEL     §1PTT9        WTT(10)
026900980504     C                   MOVEL     §1PTTV        WTT(11)
027000980504     C*
027100980317     C* CARICO TABELLA CODICI TARIFFA
027200980317     C                   EXSR      CARTAB
027300941114     C* 11 ON  - VISUALIZZO IL VALORE MERCE E IL FLAG VALORE MERCE
027400941114     C     §1PVVM        COMP      'S'                                    11
027500941114     C* 28 ON  - VISUALIZZO IL TIPO APPLICAZIONE
027600941114     C     §1PVCA        COMP      'S'                                    28
027700941116     C* 38 ON  - VISUALIZZO IL MINIMO  TARIFFA
027800941116     C     §1PVMN        COMP      'S'                                    38
027900941116     C* 39 ON  - VISUALIZZO IL MASSIMO TARIFFA
028000941116     C     §1PVMX        COMP      'S'                                    39
028100980514     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO FITTIZIO
028200990622     C     VIDFTC        COMP      'C '                                   43
028300060802     C* 82 ON  - VISUALIZZO DATA riferimento prezzo base carburante
028400060802     C     §1PDRF        COMP      'S'                                    82
028500941114    4C                   ENDIF
028600941114     C*
028700941114     C                   EXSR      VIDEO2
028800941114    3C                   END
028900920708     C*
029000940913     C     NRR           CHAIN     TA67S02                            31
029100920529     C                   MOVEL     ' '           VIDSCE
029200940913     C                   UPDATE    TA67S02
029300941114    2C                   END
029400921006     C*
029500940913     C                   READC     TA67S02                                31
029600941114    1C                   END
029700921119     C                   GOTO      INIZIO
029800920529     C*
029900920529     C     FINE          TAG
030000120321
030100120321     c                   IF        *in97
030200120321     c                   eval      parm48 = tnsb48ds
030300120321     c                   ENDIF
030400120321
030500920529     C                   SETON                                        LR
030600920609     C*
030700920609     C*--- CARICO TABELLA CODICI TARIFFA -----------------------------*
030800920609     C     CARTAB        BEGSR
030900941114     C****  CODICI TARIFFA E DESCRIZIONI  ****
031000920609     C                   MOVEL     'TR'          COD
031100920609     C                   Z-ADD     1             X                 3 0
031200980317     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
031300980317     C* PARTICOLARE
031400980317    1C     WTT(X)        DOWNE     ' '
031500980317     C                   MOVEL     WTT(X)        KEY
031600980317     C     KTAB          CHAIN     TABEL                              30
031700980317    2C     *IN30         IFEQ      '0'
031800980317     C     TBLFLG        ANDEQ     *BLANKS
031900940921     C                   MOVEL     TBLUNI        DSTR
032000940913     C*
032100980317     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
032200980317    3C     §TRUPA        IFEQ      'S'
032300920612     C                   MOVEL     TBLKEY        TTR(X)
032400941114     C                   MOVEL     §TRDES        DTR(X)
032500941114     C* §TRTAP = "S" --> TARIFFA IN %    = " " --> TARIFFA IN £
032600941114     C                   MOVEL     §TRTAP        TTP(X)
032700980520     C* ARROTONDAMENTI
032800980520     C                   MOVEL     §TRARR        ARR(X)
032900980520     C* RAPPORTO PESO VOLUME
033000980520     C                   MOVEL     §TRRPV        RPV(X)
033100940913     C                   END
033200940913     C*
033300920609     C                   END
033400980317     C*
033500980317     C                   ADD       1             X
033600980317     C                   ENDDO
033700921006     C*
033800920609     C                   ENDSR
033900080609
034000080609      *------------------------------------------------------------------------*
034100080609      * Carico filiali abilitate al lasciato avviso
034200080609      *------------------------------------------------------------------------*
034300080609     c     carlav        begsr
034400080609
034500080609     c                   clear                   sklav
034600080609     c                   clear                   xx
034700080609
034800080609     c/exec sql
034900080609     c+ declare lav cursor for select tntbe01l.*
035000080609     c+ from tntbe01l
035100080609     c+ where tbecod='LAV' and tbeatb = ''
035200080609     c/end-exec
035300080609
035400080609     c/exec sql
035500080609     c+ open lav
035600080609     c/end-exec
035700080609
035800080609     c                   do        *hival
035900080609
036000080609     c/exec sql
036100080609     c+ fetch next from lav into :tntbeds
036200080609     c/end-exec
036300080609
036400080609     c                   select
036500080609     c                   when      sqlcod = 100
036600080609     c                   leave
036700080609     c                   when      sqlcod < 0
036800080609     c                   other
036900080609     c                   eval      dlav = tbeuni
037000080609      * carico la schiera della filiali abilitate
037100080616     c                   if        d§lavtar > *date
037200080609     c                   iter
037300080609     c                   endif
037400080609     c                   eval      xx = xx + 1
037500080609     c                   eval      sklav(xx) = %dec(tbeke1:3:0)
037600080609
037700080609     c                   endsl
037800080609
037900080609     c                   enddo
038000080609
038100080609     c/exec sql
038200080609     c+ close lav
038300080609     c/end-exec
038400080609
038500080609
038600080609     c                   endsr
038700920609     C*
038800920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
038900920529     C     CARICA        BEGSR
039000920529     C                   SETOFF                                       3031
039100920529     C* PULIZIA SUBFILE
039200920529     C                   EXSR      PULSF
039300920529     C*
039400920529     C                   Z-ADD     0             NRR               4 0
039500920529     C* CODICE CONSEGNA PARTICOLARE E DESCRIZIONE
039600920529     C                   MOVEL     '1P'          COD
039700920608     C     KTAB2         SETLL     TABEL
039800920608     C     KTAB2         READE     TABEL                                  30
039900920611     C     *IN30         DOWEQ     '0'
040000920611     C* CONTROLLO VALIDITA' CODICE CONSEGNA
040100920611     C     TBLFLG        IFEQ      *BLANKS
040200920529     C                   MOVEL     TBLUNI        DS1P
040300020204      * FLAG DI COMODO PER TABELLA 'SP'
040400020204     C                   CLEAR                   FLAGSP
040500020201      *
040600080609      * ESCLUDO la tariffa particolare "c" lasciato avviso
040700080609      * per le filiali non abilitate, accetto cartello
040800051012     c                   If        tblkey = 'c       '  and
040900080609     c                             kscfil <> 888
041000080609     c                   if        %lookup(kscfil:sklav) = *zeros
041100080609     c                             and %lookup(999:sklav) = *zeros
041200051012     c                   goto      legtab
041300080609     c                   endif
041400051012     c                   endif
041500060308      * ESCLUDO la tariffa particolare "d" se tariffa di cartello
041600060308     c                   if        tblkey = 'd       '  and *in44
041700060308     c                   goto      legtab
041800060308     c                   endif
041900051012      *
042000020201      *----------------------
042100020201      * NUOVI CONTROLLI!!!!!!
042200020201      *----------------------
042300020204      * Carico solo le tariffe particolari che hanno flag <> 'N'
042400020201      * Per tariffa con tipo servizio poste controllo flag §1pfpt
042500020201     C     PARTSP        IFEQ      'P'
042600020201     C     §1PFPT        ANDEQ     'N'
042700020201     C                   GOTO      LEGTAB
042800020201     C                   ENDIF
042900020201      * Per tariffa con Network DPD controllo flag §1pfdp
043000020201     C     FLGNET        IFEQ      'D'
043100020201     C     §1PFDP        ANDEQ     'N'
043200020201     C                   GOTO      LEGTAB
043300020201     C                   ENDIF
043400020201      * Per tariffa con Network FedEx controllo flag §1pffe
043500020201     C     FLGNET        IFEQ      'F'
043600020201     C     §1PFFE        ANDEQ     'N'
043700040428      * tariffa FEDEX abilitata solo per le tariffe di cartello e 44 spento
043800040428      * (44 acceso solo se sto modificando una tariffa di cartello)
043900040428     c     flgnet        oreq      'F'
044000040428     c     §1pffe        andeq     'C'
044100040428     c     *in44         andeq     *off
044200020201     C                   GOTO      LEGTAB
044300020201     C                   ENDIF
044400020201      * Per tariffa Italia controllo flag §1ptco
044500020201     C     PARFIE        IFEQ      'I'
044600020205     C     PARTSP        ANDNE     'P'
044700020201     C     §1PTCO        ANDEQ     'N'
044800020201     C                   GOTO      LEGTAB
044900020201     C                   ENDIF
045000020201      * Per tariffa Estero controllo flag §1pfie
045100020201     C     PARFIE        IFEQ      'E'
045200020205     C     FLGNET        ANDEQ     ' '
045300020201     C     §1PFIE        ANDEQ     'N'
045400020201     C                   GOTO      LEGTAB
045500020201     C                   ENDIF
045600020225      * Per tariffe di cartello verifico che non sia tipo servizio "P"
045700020225     C     PARFIE        IFEQ      ' '
045800020225     C     PARTSP        ANDNE     'P'
045900020225     C     §1PTCO        ANDEQ     'N'
046000020225     C                   GOTO      LEGTAB
046100020225     C                   ENDIF
046200020201      *
046300941025     C*
046400941025     C* CONTROLLO SE SI TRATTA DI CONS.PARTIC. UTILIZZABILE IN TARIFFA
046500921006     C     §1PUTA        IFEQ      'S'
046600970325     C*
046700921006     C                   MOVEL     TBLKEY        VIDFTC
046800920529     C                   MOVEL     §1PDES        VIDDES
046900020201      *
047000020201      * Per tariffa FedEx controllo se devo recupare la descrizione
047100020201      * dalla tabella SP
047200020201     C     FLGNET        IFEQ      'F'
047300020201     C     §1PDFE        ANDEQ     'S'
047400020204     C                   MOVEL     '1'           FLAGSP            1
047500020201     C                   CLEAR                   DSSP
047600020201     C                   MOVEL     'SP'          COD
047700020201     C                   MOVEL     *BLANKS       KEY
047800020201     C                   MOVEL     VIDFTC        KEY
047900020201     C     KTAB          CHAIN     TABEL                              34
048000020201     C     *IN34         IFEQ      *OFF
048100020201     C                   MOVEL     TBLUNI        DSSP
048200020201     C                   ENDIF
048300020201     C     §SPDFE        IFNE      *BLANKS
048400020201     C                   MOVEL     §SPDFE        VIDDES
048500020201     C                   ENDIF
048600020201     C                   ENDIF
048700020201      *
048800920529     C* CAMPO SELEZIONE
048900920529     C                   MOVEL     ' '           VIDSCE
049000920529     C*
049100990622     C     KTPT          CHAIN     TITPT000                           31
049200990907     C                   Z-ADD     0             VIDDUV
049300920529     C     *IN31         IFEQ      '0'
049400920529     C* CONTROLLO VALIDITA' TARIFFA
049500920529     C     TPTATB        IFEQ      ' '
049600920529     C* TARIFFA PARTICOLARE GIA' ESISTENTE
049700920529     C                   MOVEL     'S'           VIDFLG
049800990907     C* VALORIZZO L'ULTIMA DATA VARIAZIONE
049900990907     C*
050000990907     C* GIRO DATA
050100990907     C*
050200990907     C                   Z-ADD     TPTDUV        G02INV
050300990907     C                   MOVEL     '3'           G02ERR
050400990907     C                   CALL      'XSRDA8'
050500990907     C                   PARM                    WLBDAT
050600990907     C*
050700990907     C                   Z-ADD     G02DAT        VIDDUV
050800990907     C*
050900920529     C                   ELSE
051000920529     C* TARIFFA ANNULLATA
051100920707     C                   MOVEL     ' '           VIDFLG
051200920529     C                   END
051300920529     C*
051400920529     C                   ELSE
051500920529     C* TARIFFA PARTICOLARE NON ESISTENTE
051600920707     C                   MOVEL     ' '           VIDFLG
051700920529     C                   END
051800920529     C*
051900990217     C* SE C'E' LA TARIFFA -->VISUALIZZO
052000130607    4C**** VIDFLG        IFEQ      'S'
052100920529     C                   ADD       1             NRR
052200940913     C                   WRITE     TA67S02
052300130607    4C****               END
052400990217    3C                   END
052500990217    2C                   END
052600920529     C*
052700000420     C     LEGTAB        TAG
052800020204     C     FLAGSP        IFNE      *BLANKS
052900020201     C                   MOVEL     '1P'          COD
053000020204     C     KTAB          SETGT     TABEL
053100020204     C                   ENDIF
053200920611     C     KTAB2         READE     TABEL                                  30
053300990217    1C                   END
053400920529     C                   Z-ADD     1             REC
053500920529     C                   ENDSR
053600920529     C*
053700920529     C*--- PULIZIA SUBFILE -------------------------------------------*
053800920529     C     PULSF         BEGSR
053900920529     C                   SETON                                        35
054000940913     C                   WRITE     TA67C02
054100920529     C                   SETOFF                                       35
054200920529     C                   ENDSR
054300920529     C*
054400920529     C*--- SR SECONDA VIDEATA ----------------------------------------*
054500920529     C     VIDEO2        BEGSR
054600941116     C                   SETOFF                                       0614
054700921006     C                   SETOFF                                       2425
054800920709     C* 37 - LO UTILIZZO PER EMETTERE IL SUBFILE VUOTO QUANDO C'E'
054900920709     C*      LA TESTATA MA NON CI SONO RIGHE DI DETTAGLIO
055000920709     C                   SETOFF                                       37
055100941114     C*
055200990217     C* 24 ON - TIPO CONSEGNA  ASSICURAZIONE X CONTO
055300990217     C**         VI2FTC    COMP 'C'                      24
055400990217     C     §1PASC        IFEQ      'S'
055500990217     C                   SETON                                            24
055600990217     C                   ENDIF
055700990217     C* 25 ON - TIPO CONSEGNA RCV
055800990217     C****       VI2FTC    COMP 'R'                      25
055900990217     C     §1PRCV        IFEQ      'S'
056000990217     C                   SETON                                            25
056100990217     C                   ENDIF
056200941114     C*
056300920611     C* PULIZIA SUBFILE
056400920611     C                   EXSR      PULSF2
056500941114     C*
056600941114     C* 06 ON - TIPO CONSEGNA = I-ISOLE MINORI
056700061220     C***  VI2FTC        COMP      'I '                                   06
056800061220     C* 06 ON - APPLICAZIONE CON INOLTRO  PER ISOLE/LOC.DISAGIATE/C.STO
056900061220     C     §1PAIN        COMP      'S'                                    06
057000921006     C* CARICO DATI SFLCONTROL
057100921006     C                   EXSR      CARCTL
057200920529     C* CARICO SUBFILE
057300941114     C                   EXSR      CARSFL
057400920617     C*
057500920617     C                   Z-ADD     1             REC2
057600940913     C                   WRITE     TA67T01
057700940913     C                   WRITE     TA67Z05
057800941114     C                   WRITE     TA67D06
057900941114     C                   EXFMT     TA67C07
058000920611     C                   ENDSR
058100920609     C*
058200920611     C*--- PULIZIA SUBFILE -------------------------------------------*
058300920611     C     PULSF2        BEGSR
058400920611     C                   SETON                                        37
058500941114     C                   WRITE     TA67C07
058600920611     C                   SETOFF                                       37
058700920611     C                   ENDSR
058800920611     C*
058900920529     C*--- CARICO DATI IN SUBFILE ------------------------------------*
059000941114     C     CARSFL        BEGSR
059100920707     C                   SETOFF                                       3233
059200920616     C*
059300920611     C                   MOVEL     'CT'          COD
059400920611     C                   MOVEL     *BLANKS       KEY
059500920529     C* RICERCA
059600990622     C     KTPT          SETLL     TITPD000
059700920608     C                   Z-ADD     0             NR2               4 0
059800990622     C     KTPT          READE     TITPD000                               32
059900920529     C* 32 ON - FINE LETTURA
060000920611     C     *IN32         DOWEQ     '0'
060100920611     C* CONTROLLO VALIDITA' TARIFFA
060200920611     C     TPDATB        IFEQ      ' '
060300920529     C* CODICE TASSAZIONE
060400920529     C                   MOVEL     TPDCTS        VI2CTS
060500920608     C* DECODIFICA CODICE TASSAZIONE
060600920609     C                   MOVEL     VI2CTS        KEY
060700920608     C     KTAB          CHAIN     TABEL                              33
060800920612     C  N33              DO
060900920612     C                   MOVEL     TBLUNI        DSCT
061000920612     C                   MOVEL     §CTDES        VI2DCT
061100920612     C                   MOVEL     §CTCOR        VI2ORP
061200920612     C                   END
061300920608     C   33              MOVEL     *BLANKS       VI2DCT
061400920615     C* SCAGLIONE
061500940713     C                   MOVEL     TPDSGL        VI2SGL
061600920608     C* IMPORTO TARIFFA
061700941114     C                   Z-ADD     TPDITR        VI2ITR
061800940713     C* MINIMO  TARIFFA
061900940713     C                   MOVEL     TPDMIN        VI2MIN
062000940713     C* MASSIMO TARIFFA
062100940713     C                   MOVEL     TPDMAX        VI2MAX
062200920611     C* 06 ON - TIPO CONSEGNA = I  CARICO FLAG TARIFFA PROVINCIA
062300920611     C   06              MOVEL     TPDAIN        VI2AIN
062400920610     C*
062500920608     C* FLAG ANNULLAMENTO
062600920610     C                   MOVEL     ' '           VI2ATB
062700920610     C*
062800920608     C                   ADD       1             NR2
062900920608     C*
063000941114     C                   WRITE     TA67S07
063100920611     C                   END
063200920608     C*
063300990622     C     KTPT          READE     TITPD000                               32
063400920611     C                   END
063500920611     C*
063600920709     C* SE NON CI SONO RIGHE DI DETTAGLIO EMETTO LA TESTATA
063700920709     C     NR2           IFEQ      0
063800920709     C* 37 - LO UTILIZZO PER EMETTERE IL SUBFILE VUOTO QUANDO C'E'
063900920709     C*      LA TESTATA MA NON CI SONO RIGHE DI DETTAGLIO
064000920709     C                   SETON                                        37
064100920709     C                   END
064200920709     C*
064300920608     C                   ENDSR
064400920608     C*
064500920609     C*--- CARICO DATI SUBFILE CONTROL -------------------------------*
064600920608     C     CARCTL        BEGSR
064700921006     C                   SETOFF                                       2627
064800050927     C                   CLEAR                   TPG1
064900050927     C                   CLEAR                   DPG1
065000050927     C                   CLEAR                   TPG2
065100050927     C                   CLEAR                   DPG2
065200050927     C                   CLEAR                   TPG3
065300050927     C                   CLEAR                   DPG3
065400050927     C                   CLEAR                   TPG4
065500050927     C                   CLEAR                   DPG4
065600050927     C                   CLEAR                   TPG5
065700050927     C                   CLEAR                   DPG5
065800050927     C                   CLEAR                   TPG6
065900050927     C                   CLEAR                   DPG6
066000050927     C                   CLEAR                   TPG7
066100050927     C                   CLEAR                   DPG7
066200050927     C                   CLEAR                   TPG8
066300050927     C                   CLEAR                   DPG8
066400941114     C*
066500941114     C****  CARICO LE DECODIFICHE DEI TIPI TARIFFA  ****
066600941114     C* CARICO I TIPI TARIFFA PRENDENDOLI DALLA TABELLA "TR"
066700941114     C     TTR(1)        IFNE      ' '
066800941114     C                   MOVEL     TTR(1)        TPG1
066900941114     C                   MOVEL     DTR(1)        DPG1
067000941114     C                   END
067100941114     C     TTR(2)        IFNE      ' '
067200941114     C                   MOVEL     TTR(2)        TPG2
067300941114     C                   MOVEL     DTR(2)        DPG2
067400941114     C                   END
067500941114     C     TTR(3)        IFNE      ' '
067600941114     C                   MOVEL     TTR(3)        TPG3
067700941114     C                   MOVEL     DTR(3)        DPG3
067800941114     C                   END
067900941114     C     TTR(4)        IFNE      ' '
068000941114     C                   MOVEL     TTR(4)        TPG4
068100941114     C                   MOVEL     DTR(4)        DPG4
068200941114     C                   END
068300941114     C     TTR(5)        IFNE      ' '
068400941114     C                   MOVEL     TTR(5)        TPG5
068500941114     C                   MOVEL     DTR(5)        DPG5
068600941114     C                   END
068700941114     C     TTR(6)        IFNE      ' '
068800941114     C                   MOVEL     TTR(6)        TPG6
068900941114     C                   MOVEL     DTR(6)        DPG6
069000941114     C                   END
069100980504     C     TTR(7)        IFNE      ' '
069200980504     C                   MOVEL     TTR(7)        TPG7
069300980504     C                   MOVEL     DTR(7)        DPG7
069400980504     C                   END
069500980504     C     TTR(8)        IFNE      ' '
069600980504     C                   MOVEL     TTR(8)        TPG8
069700980504     C                   MOVEL     DTR(8)        DPG8
069800980504     C                   END
069900941114     C*
070000941114     C* CARICO DATI SFLCONTROL
070100990622     C     KTPT          CHAIN     TITPT000                           33
070200941114    1C     *IN33         IFEQ      '0'
070300140115     c                   eval      dTPT01 = TPTflo
070400921007     C*
070500921007     C* TIPO TARIFFA/TIPO PAGAMENTO
070600921007     C                   MOVEL     TPTTPG        VI2TPG
070700980520     C* CONTROLLO IL TIPO TARIFFA   NELLA TABELLA "TR"
070800941114     C                   Z-ADD     1             X
070900941114     C     VI2TPG        LOOKUP    TTR(X)                                 20
071000941114     C* CONTROLLO SE TARIFFA IN % O IN £
071100941114     C   20TTP(X)        COMP      'S'                                    14
071200980520     C* ARROTONDAMENTI
071300980520     C   20              MOVEL     ARR(X)        WARR              1
071400980520     C* RAPPORTO PESO VOLUME
071500980520     C   20              MOVEL     RPV(X)        WRPV              1
071600941114     C*
071700980520     C* CONDIZIONO L'EMISSIONE DEGLI ARROTOND. E DEL RAPPORTO PESO/VOL.
071800980520     C     WRPV          IFEQ      'S'
071900921006     C                   SETON                                        27
072000980520     C                   END
072100980520     C     WARR          IFEQ      'S'
072200921006     C                   SETON                                        26
072300980520     C                   END
072400941114     C* 26 ON  - VISUALIZZO  ARROTONDAMENTI
072500941114    2C     *IN26         IFEQ      *ON
072600920608     C                   MOVEL     TPTARF        VI2ARF
072700940713     C                   MOVEL     TPTARL        VI2ARL
072800920608     C                   MOVEL     TPTARO        VI2ARO
072900941114    2C                   END
073000941114     C* 27 ON  - VISUALIZZO RAPPORTO PESO/VOLUME
073100921006     C   27              Z-ADD     TPTRPV        VI2RPV
073200921006     C*
073300941114     C* 11 ON  - VISUALIZZO VALORE MERCE E FLAG VALORE MERCE
073400941114    2C     *IN11         IFEQ      *ON
073500921006     C* VALORE MERCE/LIMITE RISARCIBILE
073600921006     C                   Z-ADD     TPTVLM        VI2VLM
073700920624     C* TIPO VALORE
073800940713     C                   MOVEL     TPTFVM        VI2FVM
073900921006     C*
074000980317     C* DECODIFICA TIPO VALORE MERCE DALLA TABELLA TR
074100921006     C                   MOVEL     *BLANKS       DESFVM
074200921006     C*
074300980317     C                   MOVEL     'TR'          COD
074400921006     C                   MOVEL     *BLANKS       KEY
074500921006     C                   MOVEL     VI2FVM        KEY
074600921006     C     KTAB          CHAIN     TABEL                              34
074700941114    3C     *IN34         IFEQ      '0'
074800980317     C                   MOVEL     TBLUNI        DSTR
074900980317     C     §TRUCV        IFEQ      'S'
075000980317     C                   MOVEL     §TRDES        DESFVM
075100921006     C                   END
075200941114    3C                   END
075300941114    2C                   END
075400921006     C*
075500941114     C* 28 ON  - VISUALIZZO CODICE APPLICAZIONE
075600941114     C   28              MOVEL     TPTAPL        VI2APL
075700990217     C* 43 ON  - VISUALIZZO FLAG MANDATO ASSICURATIVO
075800980504     C                   MOVE      *BLANKS       VI2MAS
075900990907     C   43              MOVEL     TPTTMA        VI2MAS
076000941114    1C                   END
076100060802      * 82 ON  - Visualizzo la data riferimento prezzo base del carburante
076200060802     c                   If        *in82
076300140224     c                   z-add     TPTDPB        vO2dpb
076400140115     c                   IF        %check(c_Digits:%subst(TPTflo:1:
076500140115     c                             %len(§TPTpma))) = *zeros
076600140115     c                   eval      VI2pma = §TPTpma
076700140115     c                   ENDIF
076800131216      * recupero il prezzo del gasolio corrispondente
076900131216     c     tptdpb        setll     tipmg01l
077000131216     c                   read      tipmg01l
077100131216     c                   if        %found(tipmg01l)
077200140224     c                   eval      vI2pmg = pmgpmg
077300140224     c                   eval      vO2sca = pmgsca
077400131216     c                   endif
077500131216
077600060802     c                   endif
077700150217
077800150219      * -?Visualizzazione "Applicazione VMA" (_/N)?
077900150219     c                   eval      VO2vma = §TPTvma
078000150219     c                   eval      *in90 = ( §TPTvma = 'N' )
078100060802
078200920608     C                   ENDSR
078300120321
078400120321      /free
078500120321       //--------------------------------------------------------------
078600120321       //?Visualizza Dati manca tariffa-
078700120321       //--------------------------------------------------------------
078800120321       BEGSR GesW01;
078900120321
079000120321         wFine = *off;
079100120321         W01dsp = ISB48dsp;
079200120321         W01tbl = ISB48tbl;
079300120321         W01lnp = ISB48lnp;
079400120321         W01lna = ISB48lna;
079500120321         W01pro = ISB48pro;
079600120321         W01cts = ISB48cts;
079700120321         W01ctr = ISB48ctr;
079800120321         W01ncl = ISB48ncl;
079900120321         W01pkf = ISB48pkf;
080000120321         W01vlf = ISB48vlf;
080100120321         W01err = ISB48err;
080200120321
080300120321         DOW wFine = *off;
080400120321           exfmt  TA67W01;
080500120321           //?- F12 = Ritorno
080600120321           IF  *inkl;
080700120321             wFine = *on;
080800120321           ENDIF;
080900120321         ENDDO;
081000120321
081100120321       ENDSR;
081200120321      /end-free
