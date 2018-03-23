000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300020322      * STAMPA MOVIMENTI UNITA' EDP - ESEGUE                                   *
000400020322      *------------------------------------------------------------------------*
000500020322
000600020322      *------------------------------------------------------------------------*
000700020322      * DATA BASE                                                              *
000800020322      *------------------------------------------------------------------------*
000900931213     FUNMOV01L  IF   E           K DISK    USROPN
001000931213     FUNMOV02L  IF   E           K DISK    USROPN
001100931213     F                                     RENAME(UNMOV000:MOV02L)
001200020531     FAZORG21l  IF   E           K DISK
001300020531     fazorg01l  if   e           k disk
001400020322     ftntbe01l  if   e           k disk
001500941115     FTNSDU9P   O    E             PRINTER OFLIND(*IN01)
001600020322
001700020322      *------------------------------------------------------------------------*
001800020322      * SCHIERE                                                                *
001900020322      *------------------------------------------------------------------------*
002000020322      *------------------
002100020322      * DECODIFICA PARAMETRI FILTRO
002200020322      *------------------
002300931214     D PAR             S             26    DIM(2) CTDATA PERRCD(1)
002400020322      *------------------
002500020322      * DECODIFICHE TIPI UNITA'
002600020322      *------------------
002700931203     D TIP             S              2  0 DIM(100)
002800931213     D DTIP            S             15    DIM(100)
002900020322
003000020322      *------------------------------------------------------------------------*
003100020322      * INPUT                                                                  *
003200020322      *------------------------------------------------------------------------*
003300960202     D KPJBA         E DS
003400960202     D  PROFIL                 1     10                                         UTENTE
003500020322      *------------------
003600020322      * DS "XSRDA8" - CONTROLLA DATA (8)
003700020322      *------------------
003800971013     D WLBDA8          DS
003900971013     D  G08DAT                 1      8  0
004000971013     D  G08INV                 9     16  0
004100971013     D  G08ERR                17     17
004200971013     D  G08TGI                18     22  0
004300020322      *------------------
004400020322      * PARAMETRI FILTRO
004500020322      *------------------
004600960202     D PARAM           DS
004700971013     D  PARORD                 1      1  0
004800971013     D  PARTIP                 2      3  0
004900971013     D  PARCOD                 4      8  0
005000971013     D  PARMAG                 9     11  0
005100971013     D  PARDTM                12     19  0
005200971013     D  PARDT2                20     27  0
005300971013     D  PARCAU                28     31
005400971013     D  DECTIP                32     61
005500971013     D  DECMAG                62     91
005600971013     D  DECDTM                82     99  0
005700971013     D  DECDT2               100    107  0
005800971013     D  DECCAU               108    127
005900020327
006000020327     d tibs69ds      e ds
006100020327     d ds_cnaco      e ds                  extname(cnaco00f)
006200020327     d ds_cnind      e ds                  extname(cnind00f)
006300020327     d ds_cnclp      e ds                  extname(cnclp00f)
006400020327     d ds_fncls      e ds                  extname(fncls00f)
006500020709
006600020709     d ddatiute      e ds
006700020709     d azuteds       e ds                  extname(azute00f)
006800020709     d tibs34ds      e ds                  inz
006900020322
007000931202     C*------------------------------------------------------------------------*
007100950126     C* MAIN LINE
007200931202     C*------------------------------------------------------------------------*
007300000000     C*
007400931007     C* STAMPA PARAMETRI FILTRO
007500931007     C                   EXSR      STAPAR
007600931209     C*
007700931209     C* SCELTA ELABORAZIONE
007800931209CS  1C     PARORD        CASEQ     1             ELORD1                         *ELABORAZIONE 1° ORD
007900931209     C     PARORD        CASEQ     2             ELORD2                         *ELABORAZIONE 2° ORD
008000931209E   1C                   ENDCS
008100931202     C*
008200931206     C* STAMPA RIGA DI FINE PROSPETTO
008300941115     C                   WRITE     SDU9FI
008400931206     C*
008500000000     C                   SETON                                        LR
008600931206     C*------------------------------------------------------------------------*
008700950126     C* ELORD1 - ELABORAZIONE MOVIMENTI RICHIESTA 1° ORDINAMENTO
008800931206     C*------------------------------------------------------------------------*
008900931206     C     ELORD1        BEGSR
009000950127     C*
009100931209     C* APERTURA FILE
009200931213     C                   OPEN      UNMOV01L
009300931206     C*
009400931206     C* POSIZIONAMENTO E PRIMA LETTURA
009500931209     C                   EXSR      SETFIL
009600931209     C*-------------------
009700931206     C* FINCHE' FINE FILE
009800931209     C*-------------------
009900931206DO  1C     $FINE         DOWEQ     '0'
010000931213     C*
010100931214     C* OPERAZIONI PER NUOVO TIPO UNITA'
010200931214     C                   EXSR      NEWTIP
010300931209     C*-------------------
010400931214     C* FINCHE' ROTTURA TIPO UNITA'
010500931209     C*-------------------
010600931206DO  2C     $FINE         DOWEQ     '0'
010700931213     C     DEPTIP        ANDEQ     UNMTIP
010800931214     C*
010900931214     C* OPERAZIONI PER NUOVO CODICE UNITA'
011000931214     C                   EXSR      NEWCOD
011100931214     C*-------------------
011200931214     C* FINCHE' ROTTURA CODICE UNITA'
011300931214     C*-------------------
011400931214DO  3C     $FINE         DOWEQ     '0'
011500931214     C     DEPTIP        ANDEQ     UNMTIP
011600931214     C     DEPCOD        ANDEQ     UNMCOD
011700931207     C*
011800931213     C* FINE PAGINA: STAMPA TESTATA
011900931214IF  4C     *IN01         IFEQ      *ON
012000931213     C                   EXSR      STATES
012100931209     C                   SETOFF                                       01        *REIMPOSTO INDICATOR
012200931214E   4C                   ENDIF
012300931213     C*
012400931213     C* STAMPO RIGA MOVIMENTO
012500931213     C                   EXSR      STADET
012600950127     C*
012700950127     C* CONTA I MOVIMENTI
012800950127     C                   ADD       1             TOTTIP                         *TOTALE X TIPO
012900950127     C                   ADD       1             TOTGEN            7 0          *TOTALE GENERALE
013000931206     C*
013100931206     C* LETTURA SUCESSIVA UNITA' EDP
013200931209     C                   EXSR      LETFIL
013300931214E   3C                   ENDDO
013400931214E   2C                   ENDDO
013500950127     C*
013600950127     C* STAMPA TOTALE TIPO
013700950127     C                   MOVEL     *BLANKS       TODTOT
013800950127     C                   MOVEL     'TIPO'        TODTOT                         *DESCRIZIONE TOTALE
013900950127     C                   Z-ADD     TOTTIP        TOCTOT                         *TOTALE
014000950127     C                   EXSR      STATOT
014100931214     C*
014200931214     C* A ROTTURA TIPO SALTO PAGINA
014300931214     C                   SETON                                        01
014400931214E   1C                   ENDDO
014500931213     C*
014600950127     C* STAMPA TOTALE GENERALE
014700950127     C                   MOVEL     *BLANKS       TODTOT
014800950127     C                   MOVEL     'GENERALE'    TODTOT                         *DESCRIZIONE TOTALE
014900950127     C                   Z-ADD     TOTGEN        TOCTOT
015000950127     C                   EXSR      STATOT
015100950127     C* CHIUSURA FILE
015200931213     C                   CLOSE     UNMOV01L
015300931213     C*
015400931206     C                   ENDSR
015500931213     C*------------------------------------------------------------------------*
015600950126     C* ELORD2 - ELABORAZIONE MOVIMENTI RICHIESTA 2° ORDINAMENTO
015700931213     C*------------------------------------------------------------------------*
015800931213     C     ELORD2        BEGSR
015900931213     C*
016000931213     C* APERTURA FILE
016100931213     C                   OPEN      UNMOV02L
016200931214     C*
016300931214     C* POSIZIONAMENTO E PRIMA LETTURA
016400931214     C                   EXSR      SETFIL
016500931214     C*-------------------
016600931214     C* FINCHE' FINE FILE
016700931214     C*-------------------
016800931214DO  1C     $FINE         DOWEQ     '0'
016900931214     C*
017000931214     C* OPERAZIONI PER NUOVO MAGAZZINO UBICAZIONE
017100931214     C                   EXSR      NEWMAG
017200931214     C*-------------------
017300931214     C* FINCHE' ROTTURA MAGAZZINO
017400931214     C*-------------------
017500931214DO  2C     $FINE         DOWEQ     '0'
017600931214     C     DEPMAG        ANDEQ     UNMMAG
017700931214     C*
017800931214     C* FINE PAGINA: STAMPA TESTATA
017900931214IF  3C     *IN01         IFEQ      *ON
018000931214     C                   EXSR      STATES
018100931214     C                   SETOFF                                       01        *REIMPOSTO INDICATOR
018200931214E   3C                   ENDIF
018300931214     C*
018400931214     C* STAMPO RIGA MOVIMENTO
018500931214     C                   EXSR      STADET
018600950127     C*
018700950127     C* CONTA I MOVIMENTI
018800950127     C                   ADD       1             TOTMAG                         *TOTALE X MAGAZZINO
018900950127     C                   ADD       1             TOTGEN                         *TOTALE GENERALE
019000931214     C*
019100931214     C* LETTURA SUCESSIVA UNITA' EDP
019200931214     C                   EXSR      LETFIL
019300931214E   2C                   ENDDO
019400950127     C*
019500950127     C* STAMPA TOTALE MAGAZZINO
019600950127     C                   MOVEL     *BLANKS       TODTOT
019700960930     C                   MOVEL(P)  'P.O.'        TODTOT                         *DESCRIZIONE FILIALE
019800950127     C                   Z-ADD     TOTMAG        TOCTOT
019900950127     C                   EXSR      STATOT
020000931214     C*
020100931214     C* A ROTTURA MAGAZZINO SALTO PAGINA
020200931214     C                   SETON                                        01
020300931214E   1C                   ENDDO
020400950127     C*
020500950127     C* STAMPA TOTALE GENERALE
020600950127     C                   MOVEL     *BLANKS       TODTOT
020700950127     C                   MOVEL     'GENERALE'    TODTOT                         *DESCRIZIONE TOTALE
020800950127     C                   Z-ADD     TOTGEN        TOCTOT
020900950127     C                   EXSR      STATOT
021000931213     C*
021100931213     C* CHIUSURA FILE
021200931213     C                   CLOSE     UNMOV02L
021300931213     C*
021400931213     C                   ENDSR
021500931209     C*------------------------------------------------------------------------*
021600950126     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA UNITA' EDP
021700931209     C*------------------------------------------------------------------------*
021800931209     C     SETFIL        BEGSR
021900931209     C*
022000931209     C* POSIZIONAMENTO 1° ORDINAMENTO
022100931209IF  1C     PARORD        IFEQ      1
022200931213     C                   Z-ADD     PARTIP        KUNTIP
022300931213     C                   Z-ADD     PARCOD        KUNCOD
022400931213     C                   Z-ADD     PARDTM        KUNDTM
022500931213     C     KEYMO1        SETLL     UNMOV01L
022600931209E   1C                   ENDIF
022700931209     C*
022800931209     C* POSIZIONAMENTO 2° ORDINAMENTO
022900931209IF  1C     PARORD        IFEQ      2
023000931213     C                   Z-ADD     PARMAG        KUNMAG
023100931213     C                   Z-ADD     PARDTM        KUNDTM
023200931213     C     KEYMO2        SETLL     UNMOV02L
023300931209E   1C                   ENDIF
023400931209     C*
023500931209     C* P R I M A   L E T T U R A
023600931210     C*
023700931209     C                   EXSR      LETFIL                                       *LETTURA RECORD
023800931209     C*
023900931209     C                   ENDSR
024000931209     C*------------------------------------------------------------------------*
024100950126     C* LETFIL - LETTURA PROSSIMO RECORD MOVIMENTI UNITA'
024200931209     C*------------------------------------------------------------------------*
024300931209     C     LETFIL        BEGSR
024400931209     C*
024500931209     C* LEGGO FINO A:
024600931209     C*     - FINE FILE
024700931209     C*     - TROVATO RECORD VALIDO
024800931209     C                   MOVEL     'N'           WRECOK
024900931209     C*
025000931209DO  1C     $FINE         DOWEQ     '0'
025100931209     C     WRECOK        ANDEQ     'N'
025200931209     C*
025300931209     C* LETTURA 1° ORDINAMENTO
025400931209IF  1C     PARORD        IFEQ      1
025500931213     C                   READ      UNMOV01L                               99
025600931209E   1C                   ENDIF
025700931209     C*
025800931209     C* LETTURA 2° ORDINAMENTO
025900931209IF  1C     PARORD        IFEQ      2
026000931213     C                   READ      UNMOV02L                               99
026100931209E   1C                   ENDIF
026200931209     C*
026300931209IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
026400931209     C                   MOVEL     '1'           $FINE
026500931209X   2C                   ELSE
026600931209     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
026700931209E   2C                   ENDIF
026800931209     C*
026900931209E   1C                   ENDDO
027000931209     C*
027100931209     C                   ENDSR
027200931202     C*------------------------------------------------------------------------*
027300950126     C* CHKREC - CONTROLLA VALIDITA' RECORD MOVIMENTI EDP
027400931202     C*------------------------------------------------------------------------*
027500930622     C     CHKREC        BEGSR
027600931206     C*
027700931213     C* °I CAMPI SELEZIONATI (TIPO, MAGAZZINO) PER I QUALI E' STATO ANCHE
027800931213     C*  RICHIESTO L'ORDINAMENTO, SE RISULTANO DIVERSI DAL CAMPO RECORD FANNO
027900931213     C*  USCIRE DAL CICLO.
028000950407     C*
028100931206     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
028200950407     C*
028300950407     C* MOVIMENTO ANNULLATO, ESCLUDO
028400950407IF  1C     UNMATB        IFNE      *BLANKS
028500950407     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028600950407E   1C                   ENDIF
028700931206     C*
028800931206     C* TIPO UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
028900931206IF  1C     PARTIP        IFGT      *ZEROS
029000950407     C     PARTIP        ANDNE     UNMTIP
029100931206IF  2C     PARORD        IFEQ      1                                            *ORDINAMENTO X TIPO
029200931206     C                   MOVEL     '1'           $FINE
029300931206X   2C                   ELSE
029400931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029500931206E   2C                   ENDIF
029600931206E   1C                   ENDIF
029700931213     C*
029800931213     C* CODICE UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
029900931213IF  1C     PARCOD        IFGT      *ZEROS
030000931213     C     PARCOD        ANDNE     UNMCOD
030100931213     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030200931213E   1C                   ENDIF
030300931206     C*
030400931206     C* MAGAZZINO UBICAZIONE DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
030500931206IF  1C     PARMAG        IFGT      *ZEROS
030600931213     C     PARMAG        ANDNE     UNMMAG
030700931213IF  2C     PARORD        IFEQ      2                                            *ORDINAMENTO X MAGAZ
030800931213     C                   MOVEL     '1'           $FINE
030900931213X   2C                   ELSE
031000931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031100931213E   2C                   ENDIF
031200931213E   1C                   ENDIF
031300931206     C*
031400931206     C* DATA SCADENZA MANUTENZIONE NON COMPRESA NEL RANGE, ESCLUDO
031500931213IF  1C     PARDTM        IFGT      *ZEROS
031600931213IF  2C     UNMDTM        IFGE      PARDTM
031700931213     C     UNMDTM        ANDLE     PARDT2
031800931206X   2C                   ELSE
031900931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032000931210E   2C                   ENDIF
032100931206E   1C                   ENDIF
032200931206     C*
032300931213     C* CAUSALE DIVERSA DA QUELLA SELEZIONATA, ESCLUDO
032400931213IF  1C     PARCAU        IFGT      *BLANKS
032500931213     C     PARCAU        ANDNE     UNMCAU
032600931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032700931206E   1C                   ENDIF
032800931202     C*
032900930622     C                   ENDSR
033000931206     C*------------------------------------------------------------------------*
033100950126     C* NEWTIP - OPERAZIONI PER NUOVO TIPO UNITA'
033200931206     C*------------------------------------------------------------------------*
033300931214     C     NEWTIP        BEGSR
033400931206     C*
033500931214     C* MEMORIZZA IL NUOVO TIPO UNITA' IN UN DEPOSITO
033600931213     C                   Z-ADD     UNMTIP        DEPTIP
033700950127     C*
033800950127     C* AZZERA IL TOTALE TIPO UNITA'
033900950127     C                   Z-ADD     *ZEROS        TOTTIP            7 0
034000931213     C*
034100931206     C                   ENDSR
034200931214     C*------------------------------------------------------------------------*
034300950126     C* NEWCOD - OPERAZIONI PER NUOVO CODICE UNITA'
034400931214     C*------------------------------------------------------------------------*
034500931214     C     NEWCOD        BEGSR
034600931214     C*
034700931214     C* MEMORIZZA IL NUOVO CODICE UNITA' IN UN DEPOSITO
034800931214     C                   Z-ADD     UNMCOD        DEPCOD
034900931214     C*
035000931214     C                   ENDSR
035100931214     C*------------------------------------------------------------------------*
035200950126     C* NEWMAG - OPERAZIONI PER NUOVO MAGAZZINO UBICAZIONE
035300931214     C*------------------------------------------------------------------------*
035400931214     C     NEWMAG        BEGSR
035500931214     C*
035600931214     C* MEMORIZZA IL NUOVO MAGAZZINO IN UN DEPOSITO
035700931214     C                   Z-ADD     UNMMAG        DEPMAG
035800950127     C*
035900950127     C* AZZERA IL TOTALE MAGAZZINO
036000950127     C                   Z-ADD     *ZEROS        TOTMAG            7 0
036100931214     C*
036200931214     C                   ENDSR
036300931206     C*------------------------------------------------------------------------*
036400950126     C* STATES - STAMPA TESTATA
036500931206     C*------------------------------------------------------------------------*
036600931206     C     STATES        BEGSR
036700931206     C*
036800941115     C                   WRITE     SDU9PT                                       *TITOLO
036900941115     C                   WRITE     SDU9RI                                       *RIGA
037000941115     C                   WRITE     SDU9PI                                       *DESCRIZIONE CAMPI
037100941115     C                   WRITE     SDU9RI2                                      *RIGA
037200931206     C*
037300931206     C                   ENDSR
037400931206     C*------------------------------------------------------------------------*
037500950126     C* STADET - STAMPA RIGHE DI DETTAGLIO
037600931206     C*------------------------------------------------------------------------*
037700931206     C     STADET        BEGSR
037800931206     C*
037900931206     C* MUOVO I CAMPI DEL FILE IN QUELLI DI STAMPA
038000931213     C                   Z-ADD     UNMTIP        PRCTIP
038100931206     C*
038200931206     C* DECODIFICO TIPO UNITA'
038300931206     C                   Z-ADD     1             I
038400950126     C                   SETOFF                                       28
038500931213     C     UNMTIP        LOOKUP    TIP(I)                                 28
038600931206IF  1C     *IN28         IFEQ      *ON
038700931206     C                   MOVEL     DTIP(I)       PRDTIP
038800931206X   1C                   ELSE
038900931206     C                   MOVEL     *ALL'*'       PRDTIP
039000931206E   1C                   ENDIF
039100931206     C*
039200931213     C                   Z-ADD     UNMCOD        PRCCOD
039300931213     C                   Z-ADD     UNMMAG        PRCMAG
039400931206     C*
039500931206     C* DECODIFICO MAGAZZINO UBICAZIONE
039600931206     C                   Z-ADD     1             I
039700950126     C                   SETOFF                                       28
039800020531     c     unmmag        chain     azorg21l
039900020531     c                   if        not %found(azorg21l)
040000020531     c     unmmag        chain     azorg01l
040100020531     c                   endif
040200020531     c                   if        %found and orgfva = *blanks
040300020531     c                   movel     orgdes        prdmag
040400020531     c                   else
040500931206     C                   MOVEL     *ALL'*'       PRDMAG
040600931206E   1C                   ENDIF
040700931206     C*
040800931213     C* ROVESCIO IN G/M/A LA DATA MOVIMENTO
040900971013     C                   Z-ADD     UNMDTM        G08INV
041000971013     C                   Z-ADD     *ZEROS        G08DAT
041100971013     C                   MOVEL     '3'           G08ERR
041200971013     C                   CALL      'XSRDA8'
041300971013     C                   PARM                    WLBDA8
041400971013     C                   Z-ADD     G08DAT        PRCDTM
041500931206     C*
041600931213     C                   MOVEL     UNMMGC        PRCMGC
041700931213     C                   MOVEL     UNMDES        PRCDES
041800931213     C                   MOVEL     UNMDOC        PRCDOC
041900931206     C*
042000931213     C* ROVESCIO IN G/M/A LA DATA DOCUMENTO
042100971013     C                   Z-ADD     UNMDDO        G08INV
042200971013     C                   Z-ADD     *ZEROS        G08DAT
042300971013     C                   MOVEL     '3'           G08ERR
042400971013     C                   CALL      'XSRDA8'
042500971013     C                   PARM                    WLBDA8
042600971013     C                   Z-ADD     G08DAT        PRCDDO
042700931206     C*
042800931213     C                   MOVEL     UNMTMO        PRCTMO
042900931213     C                   MOVEL     UNMCAU        PRCCAU
043000020322
043100020322      * cliente
043200020322     c                   eval      prcksc = unmksc
043300020322     c                   if        unmksc > *zeros
043400020322     c                   exsr      decksc
043500020322     c                   else
043600020322     c                   clear                   prdksc
043700020322     c                   endif
043800931213     C*
043900941115     C                   WRITE     SDU9PR
044000931213     C*
044100931206     C                   ENDSR
044200950127     C*------------------------------------------------------------------------*
044300950127     C* STATOT - STAMPA TOTALE
044400950127     C*------------------------------------------------------------------------*
044500950127     C     STATOT        BEGSR
044600950127     C*
044700950127     C                   WRITE     SDU9TO                                       *TOTALE
044800950127     C*
044900950127     C                   ENDSR
045000020322      *------------------------------------------------------------------------*
045100020322      * DECKSC - DECODIFICO CLIENTE
045200020322      *------------------------------------------------------------------------*
045300020322     c     decksc        begsr
045400020322
045500020327     c                   clear                   TIBS69DS
045600020327     c                   z-add     unmksc        I69kac
045700020327     c                   call      'TIBS69R'
045800020327     c                   parm                    tibs69DS
045900020327     c                   parm                    ds_cnaco
046000020327     c                   parm                    ds_cnind
046100020327     c                   parm                    ds_cnclp
046200020327     c                   parm                    ds_fncls
046300020327
046400020327     c                   if        o69err <> '1'
046500020327     c                   movel     acorag        prdksc
046600020327     c                   else
046700020327     c                   eval      prdksc = *all'*'
046800020327     c                   endif
046900020322
047000020322     c                   endsr
047100931203     C*------------------------------------------------------------------------*
047200950126     C* CARTAB - CARICA TABELLE
047300931203     C*------------------------------------------------------------------------*
047400931203     C     CARTAB        BEGSR
047500931203     C*
047600020322     C                   MOVEL     'UNT'         tbecod
047700931203     C                   Z-ADD     *ZEROS        I
047800931203     C*
047900020322     C     tbecod        SETLL     tntbe01l
048000020322     C     tbecod        READE     tntbe01l                               99
048100931203     C*
048200931203DO  1C     *IN99         DOWEQ     *OFF
048300020322IF  2C     tbeatb        IFEQ      *BLANKS
048400931213     C                   ADD       1             I                 3 0
048500020322     C                   MOVEL     tbeke1        TIP(I)                         *CODICE
048600020322     C                   MOVEL     tbeuni        DTIP(I)                        *DESCRIZIONE
048700931203E   2C                   ENDIF
048800931207     C*
048900020322     C     tbecod        READE     tntbe01l                               99
049000931203E   1C                   ENDDO
049100931203     C*
049200931203     C                   ENDSR
049300931202     C*------------------------------------------------------------------------*
049400950126     C* STAPAR - STAMPA PAGINA PARAMETRI
049500931202     C*------------------------------------------------------------------------*
049600931011     C     STAPAR        BEGSR
049700931213     C*
049800931203     C* TIPO ORDINAMENTO
049900931203IF  1C     PARORD        IFEQ      1
050000931203     C                   MOVEL     PAR(1)        PACORD
050100931203E   1C                   ENDIF
050200931203IF  1C     PARORD        IFEQ      2
050300931203     C                   MOVEL     PAR(2)        PACORD
050400931203E   1C                   ENDIF
050500931203     C*
050600931203     C* TIPO UNITA'
050700931203     C                   Z-ADD     PARTIP        PACTIP
050800931203     C                   MOVEL     DECTIP        PADTIP
050900931213     C*
051000931213     C* CODICE UNITA'
051100931213     C                   Z-ADD     PARCOD        PACCOD
051200931203     C*
051300931203     C* MAGAZZINO UBICAZIONE
051400931203     C                   Z-ADD     PARMAG        PACMAG
051500931213     C*
051600931213     C* CAUSALE MOVIMENTO
051700931213     C                   MOVEL     PARCAU        PACCAU
051800931217     C                   MOVEL     DECCAU        PADCAU
051900931203     C*
052000931213     C* DATA MOVIMENTO DAL E AL
052100931213     C                   Z-ADD     DECDTM        PACDTM
052200931213     C                   Z-ADD     DECDT2        PACDT2
052300931206     C*
052400941115     C                   WRITE     SDU9PT                                       *INTESTAZIONE
052500941115     C                   WRITE     SDU9PA                                       *PARAMENTRI
052600931202     C*
052700931011     C                   ENDSR
052800931202     C*------------------------------------------------------------------------*
052900960202     C* *INZSR - IMPOSTAZIONI INIZIALI
053000931202     C*------------------------------------------------------------------------*
053100960202     C     *INZSR        BEGSR
053200931011     C*
053300931011     C* PARAMETRI IN INPUT AL PGM
053400931011     C     *ENTRY        PLIST
053500931011     C                   PARM                    KPJBA
053600931011     C                   MOVEL     KPJBU         PARAM
053700931011     C*
053800931011     C* DEFINISCE LE VARIABILI DI PROGRAMMA
053900931213     C     *LIKE         DEFINE    UNMTIP        KUNTIP
054000931213     C     *LIKE         DEFINE    UNMCOD        KUNCOD
054100931213     C     *LIKE         DEFINE    UNMMAG        KUNMAG
054200931213     C     *LIKE         DEFINE    UNMDTM        KUNDTM
054300931213     C     *LIKE         DEFINE    UNMTIP        DEPTIP
054400931213     C     *LIKE         DEFINE    UNMMAG        DEPMAG
054500931213     C     *LIKE         DEFINE    UNMCOD        DEPCOD
054600931203     C*
054700931213     C* CHIAVE LETTURA UNMOV01L - PARZIALE
054800931213     C     KEYMO1        KLIST
054900931203     C                   KFLD                    KUNTIP                         *TIPO
055000931213     C                   KFLD                    KUNCOD                         *CODICE
055100931213     C                   KFLD                    KUNDTM                         *DATA MOVIMENTO
055200931203     C*
055300931213     C* CHIAVE LETTURA UNMOV02L - PARZIALE
055400931213     C     KEYMO2        KLIST
055500931213     C                   KFLD                    KUNMAG                         *MAGAZZINO
055600931213     C                   KFLD                    KUNDTM                         *DATA MOVIMENTO
055700931011     C*
055800931213     C* IMPOSTO A ZERO FINE CICLO DI LETTURA
055900931209     C                   MOVEL     '0'           $FINE             1            *FINE UNITA' EDP
056000931207     C                   SETON                                        01        *SALTO PAGINA
056100931203     C*
056200931203     C* CONTROLLO SE SONO IN SEDE, FILIALE DI 1° LIVELLO, FILIALE DI 2° LIVELLO
056300931203     C* E RECUPERO LA RAGIONE SOCIALE DELL'UTENTE
056400020709
056500020709     c     *dtaara       define    §azute        azuteds
056600020709     c     *dtaara       define    §datiute      ddatiute
056700020709
056800020709     c                   in(E)     *dtaara
056900020709     c                   if        %Error or rsut = *blanks
057000020709     c                   clear                   tibs34ds
057100020709     c                   call      'TIBS34R'
057200020709     c                   parm                    Tibs34Ds
057300020709     c                   in        *dtaara
057400020709     c                   endif
057500931203     C*
057600950126     C* CARICA TABELLE
057700931203     C                   EXSR      CARTAB
057800931206     C*
057900931011     C                   ENDSR
058000931202     O*------------------------------------------------------------------------*
058100931214**   PAR - DECODIFICHE PARAMETRI
058200931214TIPO/CODICE/DATA MOVIMENTO                                   1
058300931214MAGAZZINO/DATA MOVIMENTO                                     2
