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
003300020322      *------------------
003400020322      * PARAMETRI XFIL
003500020322      *------------------
003600020709     D**!!! DSFIL         E DS
003700960202     D KPJBA         E DS
003800960202     D  PROFIL                 1     10                                         UTENTE
003900020322      *------------------
004000020322      * DS "XSRDA8" - CONTROLLA DATA (8)
004100020322      *------------------
004200971013     D WLBDA8          DS
004300971013     D  G08DAT                 1      8  0
004400971013     D  G08INV                 9     16  0
004500971013     D  G08ERR                17     17
004600971013     D  G08TGI                18     22  0
004700020322      *------------------
004800020322      * PARAMETRI FILTRO
004900020322      *------------------
005000960202     D PARAM           DS
005100971013     D  PARORD                 1      1  0
005200971013     D  PARTIP                 2      3  0
005300971013     D  PARCOD                 4      8  0
005400971013     D  PARMAG                 9     11  0
005500971013     D  PARDTM                12     19  0
005600971013     D  PARDT2                20     27  0
005700971013     D  PARCAU                28     31
005800971013     D  DECTIP                32     61
005900971013     D  DECMAG                62     91
006000971013     D  DECDTM                82     99  0
006100971013     D  DECDT2               100    107  0
006200971013     D  DECCAU               108    127
006300020327
006400020327     d tibs69ds      e ds
006500020327     d ds_cnaco      e ds                  extname(cnaco00f)
006600020327     d ds_cnind      e ds                  extname(cnind00f)
006700020327     d ds_cnclp      e ds                  extname(cnclp00f)
006800020327     d ds_fncls      e ds                  extname(fncls00f)
006900020709
007000020709     d ddatiute      e ds
007100020709     d azuteds       e ds                  extname(azute00f)
007200020709     d tibs34ds      e ds                  inz
007300020322
007400931202     C*------------------------------------------------------------------------*
007500950126     C* MAIN LINE
007600931202     C*------------------------------------------------------------------------*
007700000000     C*
007800931007     C* STAMPA PARAMETRI FILTRO
007900931007     C                   EXSR      STAPAR
008000931209     C*
008100931209     C* SCELTA ELABORAZIONE
008200931209CS  1C     PARORD        CASEQ     1             ELORD1                         *ELABORAZIONE 1° ORD
008300931209     C     PARORD        CASEQ     2             ELORD2                         *ELABORAZIONE 2° ORD
008400931209E   1C                   ENDCS
008500931202     C*
008600931206     C* STAMPA RIGA DI FINE PROSPETTO
008700941115     C                   WRITE     SDU9FI
008800931206     C*
008900000000     C                   SETON                                        LR
009000931206     C*------------------------------------------------------------------------*
009100950126     C* ELORD1 - ELABORAZIONE MOVIMENTI RICHIESTA 1° ORDINAMENTO
009200931206     C*------------------------------------------------------------------------*
009300931206     C     ELORD1        BEGSR
009400950127     C*
009500931209     C* APERTURA FILE
009600931213     C                   OPEN      UNMOV01L
009700931206     C*
009800931206     C* POSIZIONAMENTO E PRIMA LETTURA
009900931209     C                   EXSR      SETFIL
010000931209     C*-------------------
010100931206     C* FINCHE' FINE FILE
010200931209     C*-------------------
010300931206DO  1C     $FINE         DOWEQ     '0'
010400931213     C*
010500931214     C* OPERAZIONI PER NUOVO TIPO UNITA'
010600931214     C                   EXSR      NEWTIP
010700931209     C*-------------------
010800931214     C* FINCHE' ROTTURA TIPO UNITA'
010900931209     C*-------------------
011000931206DO  2C     $FINE         DOWEQ     '0'
011100931213     C     DEPTIP        ANDEQ     UNMTIP
011200931214     C*
011300931214     C* OPERAZIONI PER NUOVO CODICE UNITA'
011400931214     C                   EXSR      NEWCOD
011500931214     C*-------------------
011600931214     C* FINCHE' ROTTURA CODICE UNITA'
011700931214     C*-------------------
011800931214DO  3C     $FINE         DOWEQ     '0'
011900931214     C     DEPTIP        ANDEQ     UNMTIP
012000931214     C     DEPCOD        ANDEQ     UNMCOD
012100931207     C*
012200931213     C* FINE PAGINA: STAMPA TESTATA
012300931214IF  4C     *IN01         IFEQ      *ON
012400931213     C                   EXSR      STATES
012500931209     C                   SETOFF                                       01        *REIMPOSTO INDICATOR
012600931214E   4C                   ENDIF
012700931213     C*
012800931213     C* STAMPO RIGA MOVIMENTO
012900931213     C                   EXSR      STADET
013000950127     C*
013100950127     C* CONTA I MOVIMENTI
013200950127     C                   ADD       1             TOTTIP                         *TOTALE X TIPO
013300950127     C                   ADD       1             TOTGEN            7 0          *TOTALE GENERALE
013400931206     C*
013500931206     C* LETTURA SUCESSIVA UNITA' EDP
013600931209     C                   EXSR      LETFIL
013700931214E   3C                   ENDDO
013800931214E   2C                   ENDDO
013900950127     C*
014000950127     C* STAMPA TOTALE TIPO
014100950127     C                   MOVEL     *BLANKS       TODTOT
014200950127     C                   MOVEL     'TIPO'        TODTOT                         *DESCRIZIONE TOTALE
014300950127     C                   Z-ADD     TOTTIP        TOCTOT                         *TOTALE
014400950127     C                   EXSR      STATOT
014500931214     C*
014600931214     C* A ROTTURA TIPO SALTO PAGINA
014700931214     C                   SETON                                        01
014800931214E   1C                   ENDDO
014900931213     C*
015000950127     C* STAMPA TOTALE GENERALE
015100950127     C                   MOVEL     *BLANKS       TODTOT
015200950127     C                   MOVEL     'GENERALE'    TODTOT                         *DESCRIZIONE TOTALE
015300950127     C                   Z-ADD     TOTGEN        TOCTOT
015400950127     C                   EXSR      STATOT
015500950127     C* CHIUSURA FILE
015600931213     C                   CLOSE     UNMOV01L
015700931213     C*
015800931206     C                   ENDSR
015900931213     C*------------------------------------------------------------------------*
016000950126     C* ELORD2 - ELABORAZIONE MOVIMENTI RICHIESTA 2° ORDINAMENTO
016100931213     C*------------------------------------------------------------------------*
016200931213     C     ELORD2        BEGSR
016300931213     C*
016400931213     C* APERTURA FILE
016500931213     C                   OPEN      UNMOV02L
016600931214     C*
016700931214     C* POSIZIONAMENTO E PRIMA LETTURA
016800931214     C                   EXSR      SETFIL
016900931214     C*-------------------
017000931214     C* FINCHE' FINE FILE
017100931214     C*-------------------
017200931214DO  1C     $FINE         DOWEQ     '0'
017300931214     C*
017400931214     C* OPERAZIONI PER NUOVO MAGAZZINO UBICAZIONE
017500931214     C                   EXSR      NEWMAG
017600931214     C*-------------------
017700931214     C* FINCHE' ROTTURA MAGAZZINO
017800931214     C*-------------------
017900931214DO  2C     $FINE         DOWEQ     '0'
018000931214     C     DEPMAG        ANDEQ     UNMMAG
018100931214     C*
018200931214     C* FINE PAGINA: STAMPA TESTATA
018300931214IF  3C     *IN01         IFEQ      *ON
018400931214     C                   EXSR      STATES
018500931214     C                   SETOFF                                       01        *REIMPOSTO INDICATOR
018600931214E   3C                   ENDIF
018700931214     C*
018800931214     C* STAMPO RIGA MOVIMENTO
018900931214     C                   EXSR      STADET
019000950127     C*
019100950127     C* CONTA I MOVIMENTI
019200950127     C                   ADD       1             TOTMAG                         *TOTALE X MAGAZZINO
019300950127     C                   ADD       1             TOTGEN                         *TOTALE GENERALE
019400931214     C*
019500931214     C* LETTURA SUCESSIVA UNITA' EDP
019600931214     C                   EXSR      LETFIL
019700931214E   2C                   ENDDO
019800950127     C*
019900950127     C* STAMPA TOTALE MAGAZZINO
020000950127     C                   MOVEL     *BLANKS       TODTOT
020100960930     C                   MOVEL(P)  'P.O.'        TODTOT                         *DESCRIZIONE FILIALE
020200950127     C                   Z-ADD     TOTMAG        TOCTOT
020300950127     C                   EXSR      STATOT
020400931214     C*
020500931214     C* A ROTTURA MAGAZZINO SALTO PAGINA
020600931214     C                   SETON                                        01
020700931214E   1C                   ENDDO
020800950127     C*
020900950127     C* STAMPA TOTALE GENERALE
021000950127     C                   MOVEL     *BLANKS       TODTOT
021100950127     C                   MOVEL     'GENERALE'    TODTOT                         *DESCRIZIONE TOTALE
021200950127     C                   Z-ADD     TOTGEN        TOCTOT
021300950127     C                   EXSR      STATOT
021400931213     C*
021500931213     C* CHIUSURA FILE
021600931213     C                   CLOSE     UNMOV02L
021700931213     C*
021800931213     C                   ENDSR
021900931209     C*------------------------------------------------------------------------*
022000950126     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA UNITA' EDP
022100931209     C*------------------------------------------------------------------------*
022200931209     C     SETFIL        BEGSR
022300931209     C*
022400931209     C* POSIZIONAMENTO 1° ORDINAMENTO
022500931209IF  1C     PARORD        IFEQ      1
022600931213     C                   Z-ADD     PARTIP        KUNTIP
022700931213     C                   Z-ADD     PARCOD        KUNCOD
022800931213     C                   Z-ADD     PARDTM        KUNDTM
022900931213     C     KEYMO1        SETLL     UNMOV01L
023000931209E   1C                   ENDIF
023100931209     C*
023200931209     C* POSIZIONAMENTO 2° ORDINAMENTO
023300931209IF  1C     PARORD        IFEQ      2
023400931213     C                   Z-ADD     PARMAG        KUNMAG
023500931213     C                   Z-ADD     PARDTM        KUNDTM
023600931213     C     KEYMO2        SETLL     UNMOV02L
023700931209E   1C                   ENDIF
023800931209     C*
023900931209     C* P R I M A   L E T T U R A
024000931210     C*
024100931209     C                   EXSR      LETFIL                                       *LETTURA RECORD
024200931209     C*
024300931209     C                   ENDSR
024400931209     C*------------------------------------------------------------------------*
024500950126     C* LETFIL - LETTURA PROSSIMO RECORD MOVIMENTI UNITA'
024600931209     C*------------------------------------------------------------------------*
024700931209     C     LETFIL        BEGSR
024800931209     C*
024900931209     C* LEGGO FINO A:
025000931209     C*     - FINE FILE
025100931209     C*     - TROVATO RECORD VALIDO
025200931209     C                   MOVEL     'N'           WRECOK
025300931209     C*
025400931209DO  1C     $FINE         DOWEQ     '0'
025500931209     C     WRECOK        ANDEQ     'N'
025600931209     C*
025700931209     C* LETTURA 1° ORDINAMENTO
025800931209IF  1C     PARORD        IFEQ      1
025900931213     C                   READ      UNMOV01L                               99
026000931209E   1C                   ENDIF
026100931209     C*
026200931209     C* LETTURA 2° ORDINAMENTO
026300931209IF  1C     PARORD        IFEQ      2
026400931213     C                   READ      UNMOV02L                               99
026500931209E   1C                   ENDIF
026600931209     C*
026700931209IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
026800931209     C                   MOVEL     '1'           $FINE
026900931209X   2C                   ELSE
027000931209     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
027100931209E   2C                   ENDIF
027200931209     C*
027300931209E   1C                   ENDDO
027400931209     C*
027500931209     C                   ENDSR
027600931202     C*------------------------------------------------------------------------*
027700950126     C* CHKREC - CONTROLLA VALIDITA' RECORD MOVIMENTI EDP
027800931202     C*------------------------------------------------------------------------*
027900930622     C     CHKREC        BEGSR
028000931206     C*
028100931213     C* °I CAMPI SELEZIONATI (TIPO, MAGAZZINO) PER I QUALI E' STATO ANCHE
028200931213     C*  RICHIESTO L'ORDINAMENTO, SE RISULTANO DIVERSI DAL CAMPO RECORD FANNO
028300931213     C*  USCIRE DAL CICLO.
028400950407     C*
028500931206     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
028600950407     C*
028700950407     C* MOVIMENTO ANNULLATO, ESCLUDO
028800950407IF  1C     UNMATB        IFNE      *BLANKS
028900950407     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029000950407E   1C                   ENDIF
029100931206     C*
029200931206     C* TIPO UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
029300931206IF  1C     PARTIP        IFGT      *ZEROS
029400950407     C     PARTIP        ANDNE     UNMTIP
029500931206IF  2C     PARORD        IFEQ      1                                            *ORDINAMENTO X TIPO
029600931206     C                   MOVEL     '1'           $FINE
029700931206X   2C                   ELSE
029800931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029900931206E   2C                   ENDIF
030000931206E   1C                   ENDIF
030100931213     C*
030200931213     C* CODICE UNITA' DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
030300931213IF  1C     PARCOD        IFGT      *ZEROS
030400931213     C     PARCOD        ANDNE     UNMCOD
030500931213     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030600931213E   1C                   ENDIF
030700931206     C*
030800931206     C* MAGAZZINO UBICAZIONE DIVERSO DA QUELLO SELEZIONATO, ESCLUDO
030900931206IF  1C     PARMAG        IFGT      *ZEROS
031000931213     C     PARMAG        ANDNE     UNMMAG
031100931213IF  2C     PARORD        IFEQ      2                                            *ORDINAMENTO X MAGAZ
031200931213     C                   MOVEL     '1'           $FINE
031300931213X   2C                   ELSE
031400931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031500931213E   2C                   ENDIF
031600931213E   1C                   ENDIF
031700931206     C*
031800931206     C* DATA SCADENZA MANUTENZIONE NON COMPRESA NEL RANGE, ESCLUDO
031900931213IF  1C     PARDTM        IFGT      *ZEROS
032000931213IF  2C     UNMDTM        IFGE      PARDTM
032100931213     C     UNMDTM        ANDLE     PARDT2
032200931206X   2C                   ELSE
032300931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032400931210E   2C                   ENDIF
032500931206E   1C                   ENDIF
032600931206     C*
032700931213     C* CAUSALE DIVERSA DA QUELLA SELEZIONATA, ESCLUDO
032800931213IF  1C     PARCAU        IFGT      *BLANKS
032900931213     C     PARCAU        ANDNE     UNMCAU
033000931206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
033100931206E   1C                   ENDIF
033200931202     C*
033300930622     C                   ENDSR
033400931206     C*------------------------------------------------------------------------*
033500950126     C* NEWTIP - OPERAZIONI PER NUOVO TIPO UNITA'
033600931206     C*------------------------------------------------------------------------*
033700931214     C     NEWTIP        BEGSR
033800931206     C*
033900931214     C* MEMORIZZA IL NUOVO TIPO UNITA' IN UN DEPOSITO
034000931213     C                   Z-ADD     UNMTIP        DEPTIP
034100950127     C*
034200950127     C* AZZERA IL TOTALE TIPO UNITA'
034300950127     C                   Z-ADD     *ZEROS        TOTTIP            7 0
034400931213     C*
034500931206     C                   ENDSR
034600931214     C*------------------------------------------------------------------------*
034700950126     C* NEWCOD - OPERAZIONI PER NUOVO CODICE UNITA'
034800931214     C*------------------------------------------------------------------------*
034900931214     C     NEWCOD        BEGSR
035000931214     C*
035100931214     C* MEMORIZZA IL NUOVO CODICE UNITA' IN UN DEPOSITO
035200931214     C                   Z-ADD     UNMCOD        DEPCOD
035300931214     C*
035400931214     C                   ENDSR
035500931214     C*------------------------------------------------------------------------*
035600950126     C* NEWMAG - OPERAZIONI PER NUOVO MAGAZZINO UBICAZIONE
035700931214     C*------------------------------------------------------------------------*
035800931214     C     NEWMAG        BEGSR
035900931214     C*
036000931214     C* MEMORIZZA IL NUOVO MAGAZZINO IN UN DEPOSITO
036100931214     C                   Z-ADD     UNMMAG        DEPMAG
036200950127     C*
036300950127     C* AZZERA IL TOTALE MAGAZZINO
036400950127     C                   Z-ADD     *ZEROS        TOTMAG            7 0
036500931214     C*
036600931214     C                   ENDSR
036700931206     C*------------------------------------------------------------------------*
036800950126     C* STATES - STAMPA TESTATA
036900931206     C*------------------------------------------------------------------------*
037000931206     C     STATES        BEGSR
037100931206     C*
037200941115     C                   WRITE     SDU9PT                                       *TITOLO
037300941115     C                   WRITE     SDU9RI                                       *RIGA
037400941115     C                   WRITE     SDU9PI                                       *DESCRIZIONE CAMPI
037500941115     C                   WRITE     SDU9RI2                                      *RIGA
037600931206     C*
037700931206     C                   ENDSR
037800931206     C*------------------------------------------------------------------------*
037900950126     C* STADET - STAMPA RIGHE DI DETTAGLIO
038000931206     C*------------------------------------------------------------------------*
038100931206     C     STADET        BEGSR
038200931206     C*
038300931206     C* MUOVO I CAMPI DEL FILE IN QUELLI DI STAMPA
038400931213     C                   Z-ADD     UNMTIP        PRCTIP
038500931206     C*
038600931206     C* DECODIFICO TIPO UNITA'
038700931206     C                   Z-ADD     1             I
038800950126     C                   SETOFF                                       28
038900931213     C     UNMTIP        LOOKUP    TIP(I)                                 28
039000931206IF  1C     *IN28         IFEQ      *ON
039100931206     C                   MOVEL     DTIP(I)       PRDTIP
039200931206X   1C                   ELSE
039300931206     C                   MOVEL     *ALL'*'       PRDTIP
039400931206E   1C                   ENDIF
039500931206     C*
039600931213     C                   Z-ADD     UNMCOD        PRCCOD
039700931213     C                   Z-ADD     UNMMAG        PRCMAG
039800931206     C*
039900931206     C* DECODIFICO MAGAZZINO UBICAZIONE
040000931206     C                   Z-ADD     1             I
040100950126     C                   SETOFF                                       28
040200020531     c     unmmag        chain     azorg21l
040300020531     c                   if        not %found(azorg21l)
040400020531     c     unmmag        chain     azorg01l
040500020531     c                   endif
040600020531     c                   if        %found and orgfva = *blanks
040700020531     c                   movel     orgdes        prdmag
040800020531     c                   else
040900931206     C                   MOVEL     *ALL'*'       PRDMAG
041000931206E   1C                   ENDIF
041100931206     C*
041200931213     C* ROVESCIO IN G/M/A LA DATA MOVIMENTO
041300971013     C                   Z-ADD     UNMDTM        G08INV
041400971013     C                   Z-ADD     *ZEROS        G08DAT
041500971013     C                   MOVEL     '3'           G08ERR
041600971013     C                   CALL      'XSRDA8'
041700971013     C                   PARM                    WLBDA8
041800971013     C                   Z-ADD     G08DAT        PRCDTM
041900931206     C*
042000931213     C                   MOVEL     UNMMGC        PRCMGC
042100931213     C                   MOVEL     UNMDES        PRCDES
042200931213     C                   MOVEL     UNMDOC        PRCDOC
042300931206     C*
042400931213     C* ROVESCIO IN G/M/A LA DATA DOCUMENTO
042500971013     C                   Z-ADD     UNMDDO        G08INV
042600971013     C                   Z-ADD     *ZEROS        G08DAT
042700971013     C                   MOVEL     '3'           G08ERR
042800971013     C                   CALL      'XSRDA8'
042900971013     C                   PARM                    WLBDA8
043000971013     C                   Z-ADD     G08DAT        PRCDDO
043100931206     C*
043200931213     C                   MOVEL     UNMTMO        PRCTMO
043300931213     C                   MOVEL     UNMCAU        PRCCAU
043400020322
043500020322      * cliente
043600020322     c                   eval      prcksc = unmksc
043700020322     c                   if        unmksc > *zeros
043800020322     c                   exsr      decksc
043900020322     c                   else
044000020322     c                   clear                   prdksc
044100020322     c                   endif
044200931213     C*
044300941115     C                   WRITE     SDU9PR
044400931213     C*
044500931206     C                   ENDSR
044600950127     C*------------------------------------------------------------------------*
044700950127     C* STATOT - STAMPA TOTALE
044800950127     C*------------------------------------------------------------------------*
044900950127     C     STATOT        BEGSR
045000950127     C*
045100950127     C                   WRITE     SDU9TO                                       *TOTALE
045200950127     C*
045300950127     C                   ENDSR
045400020322      *------------------------------------------------------------------------*
045500020322      * DECKSC - DECODIFICO CLIENTE
045600020322      *------------------------------------------------------------------------*
045700020322     c     decksc        begsr
045800020322
045900020327     c                   clear                   TIBS69DS
046000020327     c                   z-add     unmksc        I69kac
046100020327     c                   call      'TIBS69R'
046200020327     c                   parm                    tibs69DS
046300020327     c                   parm                    ds_cnaco
046400020327     c                   parm                    ds_cnind
046500020327     c                   parm                    ds_cnclp
046600020327     c                   parm                    ds_fncls
046700020327
046800020327     c                   if        o69err <> '1'
046900020327     c                   movel     acorag        prdksc
047000020327     c                   else
047100020327     c                   eval      prdksc = *all'*'
047200020327     c                   endif
047300020322
047400020322     c                   endsr
047500931203     C*------------------------------------------------------------------------*
047600950126     C* CARTAB - CARICA TABELLE
047700931203     C*------------------------------------------------------------------------*
047800931203     C     CARTAB        BEGSR
047900931203     C*
048000020322     C                   MOVEL     'UNT'         tbecod
048100931203     C                   Z-ADD     *ZEROS        I
048200931203     C*
048300020322     C     tbecod        SETLL     tntbe01l
048400020322     C     tbecod        READE     tntbe01l                               99
048500931203     C*
048600931203DO  1C     *IN99         DOWEQ     *OFF
048700020322IF  2C     tbeatb        IFEQ      *BLANKS
048800931213     C                   ADD       1             I                 3 0
048900020322     C                   MOVEL     tbeke1        TIP(I)                         *CODICE
049000020322     C                   MOVEL     tbeuni        DTIP(I)                        *DESCRIZIONE
049100931203E   2C                   ENDIF
049200931207     C*
049300020322     C     tbecod        READE     tntbe01l                               99
049400931203E   1C                   ENDDO
049500931203     C*
049600931203     C                   ENDSR
049700931202     C*------------------------------------------------------------------------*
049800950126     C* STAPAR - STAMPA PAGINA PARAMETRI
049900931202     C*------------------------------------------------------------------------*
050000931011     C     STAPAR        BEGSR
050100931213     C*
050200931203     C* TIPO ORDINAMENTO
050300931203IF  1C     PARORD        IFEQ      1
050400931203     C                   MOVEL     PAR(1)        PACORD
050500931203E   1C                   ENDIF
050600931203IF  1C     PARORD        IFEQ      2
050700931203     C                   MOVEL     PAR(2)        PACORD
050800931203E   1C                   ENDIF
050900931203     C*
051000931203     C* TIPO UNITA'
051100931203     C                   Z-ADD     PARTIP        PACTIP
051200931203     C                   MOVEL     DECTIP        PADTIP
051300931213     C*
051400931213     C* CODICE UNITA'
051500931213     C                   Z-ADD     PARCOD        PACCOD
051600931203     C*
051700931203     C* MAGAZZINO UBICAZIONE
051800931203     C                   Z-ADD     PARMAG        PACMAG
051900931213     C*
052000931213     C* CAUSALE MOVIMENTO
052100931213     C                   MOVEL     PARCAU        PACCAU
052200931217     C                   MOVEL     DECCAU        PADCAU
052300931203     C*
052400931213     C* DATA MOVIMENTO DAL E AL
052500931213     C                   Z-ADD     DECDTM        PACDTM
052600931213     C                   Z-ADD     DECDT2        PACDT2
052700931206     C*
052800941115     C                   WRITE     SDU9PT                                       *INTESTAZIONE
052900941115     C                   WRITE     SDU9PA                                       *PARAMENTRI
053000931202     C*
053100931011     C                   ENDSR
053200931202     C*------------------------------------------------------------------------*
053300960202     C* *INZSR - IMPOSTAZIONI INIZIALI
053400931202     C*------------------------------------------------------------------------*
053500960202     C     *INZSR        BEGSR
053600931011     C*
053700931011     C* PARAMETRI IN INPUT AL PGM
053800931011     C     *ENTRY        PLIST
053900931011     C                   PARM                    KPJBA
054000931011     C                   MOVEL     KPJBU         PARAM
054100931011     C*
054200931011     C* DEFINISCE LE VARIABILI DI PROGRAMMA
054300931213     C     *LIKE         DEFINE    UNMTIP        KUNTIP
054400931213     C     *LIKE         DEFINE    UNMCOD        KUNCOD
054500931213     C     *LIKE         DEFINE    UNMMAG        KUNMAG
054600931213     C     *LIKE         DEFINE    UNMDTM        KUNDTM
054700931213     C     *LIKE         DEFINE    UNMTIP        DEPTIP
054800931213     C     *LIKE         DEFINE    UNMMAG        DEPMAG
054900931213     C     *LIKE         DEFINE    UNMCOD        DEPCOD
055000931203     C*
055100931213     C* CHIAVE LETTURA UNMOV01L - PARZIALE
055200931213     C     KEYMO1        KLIST
055300931203     C                   KFLD                    KUNTIP                         *TIPO
055400931213     C                   KFLD                    KUNCOD                         *CODICE
055500931213     C                   KFLD                    KUNDTM                         *DATA MOVIMENTO
055600931203     C*
055700931213     C* CHIAVE LETTURA UNMOV02L - PARZIALE
055800931213     C     KEYMO2        KLIST
055900931213     C                   KFLD                    KUNMAG                         *MAGAZZINO
056000931213     C                   KFLD                    KUNDTM                         *DATA MOVIMENTO
056100931011     C*
056200931213     C* IMPOSTO A ZERO FINE CICLO DI LETTURA
056300931209     C                   MOVEL     '0'           $FINE             1            *FINE UNITA' EDP
056400931207     C                   SETON                                        01        *SALTO PAGINA
056500931203     C*
056600931203     C* CONTROLLO SE SONO IN SEDE, FILIALE DI 1° LIVELLO, FILIALE DI 2° LIVELLO
056700931203     C* E RECUPERO LA RAGIONE SOCIALE DELL'UTENTE
056800020709     C**!!!              CALL      'XFIL'
056900020709     C**!!!              PARM                    DSFIL
057000020709     C**!!!              Z-ADD     DSFICD        CODUT             1 0
057100020709
057200020709     c     *dtaara       define    §azute        azuteds
057300020709     c     *dtaara       define    §datiute      ddatiute
057400020709
057500020709     c                   in(E)     *dtaara
057600020709     c                   if        %Error or rsut = *blanks
057700020709     c                   clear                   tibs34ds
057800020709     c                   call      'TIBS34R'
057900020709     c                   parm                    Tibs34Ds
058000020709     c                   in        *dtaara
058100020709     c                   endif
058200931203     C*
058300950126     C* CARICA TABELLE
058400931203     C                   EXSR      CARTAB
058500931206     C*
058600931011     C                   ENDSR
058700931202     O*------------------------------------------------------------------------*
058800931214**   PAR - DECODIFICHE PARAMETRI
058900931214TIPO/CODICE/DATA MOVIMENTO                                   1
059000931214MAGAZZINO/DATA MOVIMENTO                                     2
