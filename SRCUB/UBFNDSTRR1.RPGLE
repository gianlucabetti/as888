000100970526     H*------------------------------------------------------------------------*
000200140114     H* Lancio Diagnostica formati TIT per posizione campi chiave
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800140116     FUBFMTTITD1cf   e             WORKSTN INFDS(DEVDS1)
000900140114     F                                     SFILE(FMTTITDMS:MSGULT)
001000140120     FWFTIDTMP  UF   E             DISK    usropn
001100140818     Fqsysprt   O    f  132        PRINTER usropn
001200140818     F                                     oflind(*inoa)
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D* MESSAGGI
001700970526     D*------------
001800000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001900970526     D*------------
002000970526     D* DESCRIZIONI TASTI FUNZIONALI
002100970526     D*------------
002200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* FUNZIONI DA CARICARE A VIDEO
002500970526     D*------------
002600970526     D FUV             S             19    DIM(24)
002700970526     D*------------------------------------------------------------------------*
002800970526     D* INPUT
002900970526     D*------------------------------------------------------------------------*
003000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003100970526     D*------------
003200970526     D FUDDS           DS
003300970526     D  FUDDE1                 1      1
003400970526     D  FUDFUN                 2      3
003500970526     D  FUDDE2                 4     19
003600970526     D  FUDFI0                20     34
003700970526     D  FUDATT                35     35
003800970526     D  FUDFI1                36     80
003900970526     D  FUDDES                 1     19
004000970526     D  FUDRIG                 1     80
004100970526     D*------------
004200970526     D* DS GESTIONE TASTI FUNZIONALI
004300970526     D*------------
004400970526     D DSKEY         E DS
004500970526     D DEVDS1          DS
004600970526     D  KEY                  369    369
004700970526     D*------------
004800970526     D* DS "XMSG" - GESTIONE MESSAGGI
004900970526     D*------------
005000970526     D DSMSG         E DS
005100970528     D*------------------
005200970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300970528     D*------------------
005400970528     D WLBDA8          DS                  INZ
005500970528     D  G08DAT                 1      8  0
005600970528     D  G08INV                 9     16  0
005700970528     D  G08ERR                17     17
005800970528     D  G08TGI                18     22  0
005900970526     D*------------
006000970526     D* ARCHITETTURA
006100970526     D*------------
006200970526     D KPJBA         E DS
006300020923     D*-------------------
006400020923     D* DS REPERIMENTO DATI UTENTE
006500020923     D*-------------------
006600020923     D TIBS34DS      E DS                                                       *Profili utente
006700020923     D DDATIUTE      E DS                                                       *Dati utente
006800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900120927     D* DS REPERIMENTO PADRE/FIGLI
007000120927     D TIBS10DS      E DS
007100120928     D*-------------------
007200120928
007300121003     d Status         sds
007400121003     d  SDSpgm           *proc
007500121003     d  SDSprm           *parms
007600121003     d  SDSuser              254    263
007700121003
007800140116     d pIn_Lib         s             10a
007900140116     d pIn_File        s             10a
008000140818     d pIn_Mbr         s             10a
008100140116     d pIn_PosRSD      s              4s 0
008200140116     d pIn_PosIND      s              4s 0
008300140116     d pIn_PosLOD      s              4s 0
008400140116     d pIn_PosPRD      s              4s 0
008500140116     d pIn_PosCAD      s              4s 0
008600140312     d pIn_PoTNOTE     s              4s 0
008700140312     d pIn_PoTNOTI     s              4s 0
008800150407     d pIn_VAORSR      s              4s 0
008900150407     d pIn_VAOTER      s              4s 0
009000150407     d pIn_VAORFA      s              4s 0
009100140116     d pIn_SelTxt      s             10a
009200140117     d pOut_Esito      s              2  0
009300130116
009400130116     d ora             s               t
009500140117     d wX              s              2s 0
009600150407     d wVAB            s               n
009700150407     d wVAO            s               n
009800120928
009900970526     C*------------------------------------------------------------------------*
010000000303     C* MAIN LINES
010100970526     C*------------------------------------------------------------------------*
010200970526     C*
010300120927     C* metto i default su D03
010400120927     C                   EXSR      InzD03
010500120927     C*
010600120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010700991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010800970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010900970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011000970526     C*
011100970526     C* EMETTE FORMATO VIDEO
011200970526     C                   EXSR      BUTFMR
011300970526     C*
011400970526     C* GESTIONE TASTI FUNZIONALI
011500970526     C                   EXSR      FUNCON
011600970526E   2C                   ENDDO
011700970526     C*
011800970526     C* FINE PGM
011900970526     C                   SETON                                        LR
012000970526     C*------------------------------------------------------------------------*
012100970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012200970526     C*------------------------------------------------------------------------*
012300970526     C     BUTFMR        BEGSR
012400970526     C*
012500970526     C* EMETTE LA TESTATA
012600140114     C                   WRITE     FMTTITD01
012700970526     C*
012800970526     C* SPEDISCE EVENTUALE MESSAGGIO
012900970526     C                   EXSR      SNDMSG
013000970526     C*
013100970526     C* EMETTE LE FUNZIONI
013200140114     C                   WRITE     FMTTITD02
013300970526     C*
013400970526     C* EMETTE IL VIDEO
013500140114     C                   EXFMT     FMTTITD03
013600970526     C*
013700991214     C* REIMPOSTA FLAG DI ERRORE
013800991214     C                   MOVEL     '0'           ERRL00
013900970526     C*
014000970526     C* REIMPOSTA INDICATORI DI ERRORE
014100020611     C                   SETOFF                                       555556
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500120927     C* InzD03 - Inizializzo D03
014600970526     C*------------------------------------------------------------------------*
014700120927     C     InzD03        BEGSR
014800120927     C*
014900140117     C                   EVAL      VIDLib = 'GAITRASRC'
015000140117     C                   EVAL      VIDFile= 'SRCTIT'
015100130108     C*
015200120927     C                   ENDSR
015300120927     C*------------------------------------------------------------------------*
015400120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015500120927     C*------------------------------------------------------------------------*
015600120927     C     SNDMSG        BEGSR
015700120927     C*
015800970526     C* PULISCE IL SFILE MESSAGGI
015900970526     C                   MOVEL     '1'           *IN40
016000140114     C                   WRITE     FMTTITDMC
016100970526     C                   MOVEL     '0'           *IN40
016200970526     C*
016300970526     C* VISUALIZZA IL MESSAGGIO
016400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016500970526     C                   CALL      'XMSG'        XMSGPA
016600020611     C     DSMSER        IFEQ      *ZEROS
016700970526     C                   MOVEL     DSMSKE        MSGKEY
016800020611     C                   ENDIF
016900970526     C                   Z-ADD     1             MSGULT
017000140114     C                   WRITE     FMTTITDMS
017100140114     C                   WRITE     FMTTITDMC
017200970526     C*
017300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
017400970526     C                   MOVEL     *BLANKS       DSMSMS
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* ENTGES - GESTIONE ENTER -> INVIO
017900970526     C*------------------------------------------------------------------------*
018000970526     C     ENTGES        BEGSR
018100970526     C*
018200970526     C* CONTROLLA I CAMPI IMMESSI
018300970526     C                   EXSR      DECODI
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* F03GES - GESTIONE F03 -> FINE
018800970526     C*------------------------------------------------------------------------*
018900970526     C     F03GES        BEGSR
019000970526     C*
019100970526     C* IMPOSTA "PREMUTO F03"
019200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600991214     C* F06GES - GESTIONE F06 -> CONFERMA
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F06GES        BEGSR
019900970526     C*
020000991214     C* CONTROLLA I CAMPI IMMESSI
020100991214     C                   EXSR      DECODI
020200020611     C*
020300991214     C* ESEGUE ROUTINE DI CONFERMA
020400020611     C     ERRL00        IFEQ      '0'
020500991214     C                   EXSR      ESEGUI
020600130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli esiti
020700121109     C                   MOVEL     MSG(20)       DSMSMS
020800121109     C* non esco mai dal pgm
020900121109     C***                MOVEL     '1'           FINL00
021000121108     C                   ENDIF
021100970526     C*
021200991214     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400130109     C* F07GES - GESTIONE F07 -> STAMPA
021500970526     C*------------------------------------------------------------------------*
021600130109     C     F07GES        BEGSR
021700970526     C*
021800140818     C                   EXSR      F06GES
021900130110     C*
022000970526     C                   ENDSR
022100130109     C*------------------------------------------------------------------------*
022200130109     C* F12GES - GESTIONE F12 -> RITORNO
022300130109     C*------------------------------------------------------------------------*
022400130109     C     F12GES        BEGSR
022500130109     C*
022600130109     C* IMPOSTA "PREMUTO F12"
022700130109     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022800130109     C*
022900130109     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023200970526     C*------------------------------------------------------------------------*
023300970526     C     F24GES        BEGSR
023400970526     C*
023500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023600970526     C*
023700970526     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900140116     C* ESEGUI - OPERAZIONI DI LANCIO PGM
024000970526     C*------------------------------------------------------------------------*
024100991214     C     ESEGUI        BEGSR
024200140120     C*
024300140120     C* creo il file temporaneo con elenco dei membri
024400140116     C                   EVAL      pIn_Lib     = VIDLib
024500140116     C                   EVAL      pIn_File    = VIDFile
024600140818     C                   EVAL      pIn_Mbr     = VIDMbr
024700140116     C                   EVAL      pIn_PosRSD  = VIDPosRSD
024800140116     C                   EVAL      pIn_PosIND  = VIDPosIND
024900140116     C                   EVAL      pIn_PosLOD  = VIDPosLOD
025000140116     C                   EVAL      pIn_PosPRD  = VIDPosPRD
025100140116     C                   EVAL      pIn_PosCAD  = VIDPosCAD
025200140312     C                   EVAL      pIn_PoTNOTE = VIDPoTNOTE
025300140312     C                   EVAL      pIn_PoTNOTI = VIDPoTNOTI
025400150407     C                   EVAL      pIn_VAORSR  = VIDVAORSR
025500150407     C                   EVAL      pIn_VAOTER  = VIDVAOTER
025600150407     C                   EVAL      pIn_VAORFA  = VIDVAORFA
025700140116     C                   EVAL      pIn_SelTxt  = VIDSelTxt
025800140109     c                   clear                   pOut_Esito
025900121108     C* chiamo
026000140116     C                   CALL      'UBFMTTITC2'
026100140117     c                   parm                    pIn_Lib
026200140117     c                   parm                    pIn_File
026300140818     c                   parm                    pIn_Mbr
026400140116     c                   parm                    pIn_PosRSD
026500140116     c                   parm                    pIn_PosIND
026600140116     c                   parm                    pIn_PosLOD
026700140116     c                   parm                    pIn_PosPRD
026800140116     c                   parm                    pIn_PosCAD
026900140312     c                   parm                    pIn_PoTNOTE
027000140312     c                   parm                    pIn_PoTNOTI
027100150407     C                   PARM                    pIn_VAORSR
027200150407     C                   PARM                    pIn_VAOTER
027300150407     C                   PARM                    pIn_VAORFA
027400140116     c                   parm                    pIn_SelTxt
027500140116     c                   parm                    pOut_Esito
027600121108     C*
027700140109     c                   EVAL      VIDEsito = pOut_Esito
027800140818     C* se sono in stampa
027900140818     C     FUNKEY        IFEQ      '07'
028000140818     C* apro il PRTF
028100140818     C                   IF        not %open(qsysprt)
028200140818     C                   open      qsysprt
028300140818     C                   eval      ora = %time()
028400140818     C                   ENDIF
028500140818     C* stampo testata
028600140818     C                   except    testdett
028700140818     C                   ENDIF
028800140116     C*
028900140116     C* se l'esito è 0 (cioè non ci sono stati errori che hanno bloccato la diagnostica)
029000140116     C* leggo il file WFTIDTMP per sapere il risultato
0291001401171    c                   IF        pOut_Esito = 0
029200140116     C                   OPEN      WFTIDTMP
0293001401202    C                   EVAL      wX = 0
0294001401202    C                   DOU       %eof(WFTIDTMP)
029500140117     C                   READ      WFTID000
0296001401173    C                   IF        %eof
029700140117     C                   LEAVE
0298001401173-   C                   ENDIF
029900150407     C* se sono state trovate tutte le posizioni obb. per VAB
030000150407     C*  e sono state trovate quelle facoltative la cui posizione è indicata
0301001401203    C                   IF        %subst(TIDResult:1:3) = 'XXX'
0302001403123    C                             and (%subst(TIDResult:4:1) = 'X'
030300140312     C                                  and pIn_PoTNOTE > 0
0304001403123    C                                  or  %subst(TIDResult:4:1) = ' '
030500140312     C                                  and pIn_PoTNOTE = 0)
0306001403123    C                             and (%subst(TIDResult:5:1) = 'X'
030700140312     C                                  and pIn_PoTNOTI > 0
0308001403123    C                                  or  %subst(TIDResult:5:1) = ' '
030900140312     C                                  and pIn_PoTNOTI = 0)
031000150407     C                             OR
031100150407     C* se sono state trovate tutte le posizioni obb. per VAO
031200150407     C*  e sono state trovate quelle facoltative la cui posizione è indicata
0313001504073    C                             %subst(TIDResult:6:1) = 'X'
0314001504073    C                             and (%subst(TIDResult:7:1) = 'X'
031500150407     C                                  and pIn_VAOTER > 0
0316001504073    C                                  or  %subst(TIDResult:7:1) = ' '
031700150407     C                                  and pIn_VAOTER = 0)
0318001504083    C                             and (%subst(TIDResult:8:1) = 'X'
031900150407     C                                  and pIn_VAORFA > 0
0320001504083    C                                  or  %subst(TIDResult:8:1) = ' '
032100150407     C                                  and pIn_VAORFA = 0)
032200150407     C* considero pienamente trovato il sorgente
0323001401202    C                   EVAL      wX = wX + 1
0324001401203    C                   SELECT
032500140117     C                   WHEN      WX = 1
032600140116     C                   EVAL      VIDUti01 = TIDNome
032700140117     C                   EVAL      VDDPGM01 = TIDDescr
032800140117     C                   WHEN      WX = 2
032900140117     C                   EVAL      VIDUti02 = TIDNome
033000140117     C                   EVAL      VDDPGM02 = TIDDescr
033100140117     C                   WHEN      WX = 3
033200140117     C                   EVAL      VIDUti03 = TIDNome
033300140117     C                   EVAL      VDDPGM03 = TIDDescr
033400140117     C                   WHEN      WX = 4
033500140117     C                   EVAL      VIDUti04 = TIDNome
033600140117     C                   EVAL      VDDPGM04 = TIDDescr
033700140117     C                   WHEN      WX = 5
033800140117     C                   EVAL      VIDUti05 = TIDNome
033900140117     C                   EVAL      VDDPGM05 = TIDDescr
034000140117     C                   WHEN      WX = 6
034100140117     C                   EVAL      VIDUti06 = TIDNome
034200140117     C                   EVAL      VDDPGM06 = TIDDescr
034300140117     C                   WHEN      WX = 7
034400140117     C                   EVAL      VIDUti07 = TIDNome
034500140117     C                   EVAL      VDDPGM07 = TIDDescr
034600140120     C* per ora ho solo 7 campi risultato
034700140120     C                   WHEN      WX > 7
034800140120     C                   EVAL      VIDMSG = 'Sono più di 7'
0349001401173-   C                   ENDSL
035000140818     C* se sono in stampa
035100140818     C     FUNKEY        IFEQ      '07'
035200140818     C* stampo testata per salto pagina
035300140818     C                   IF        *inoa = *on
035400140818     C                   except    testdett
035500140818     C                   ENDIF
035600140818     C* stampo il dettaglio
035700140818     C                   EXCEPT    dettaglio
0358001401203-   C                   ENDIF
0359001408183-   C                   ENDIF
036000140312     C                   DELETE    WFTID000
0361001401202-   C                   ENDDO
036200140120     C                   UNLOCK    WFTIDTMP
036300140120     C                   CLOSE     WFTIDTMP
036400140818     C* se sono in stampa
036500140818     C     FUNKEY        IFEQ      '07'
036600140818     C* chiudo il PRTF
036700140818     C                   IF        %open(qsysprt)
036800140818     C                   close     qsysprt
036900140818     C                   ENDIF
037000140818     C                   ENDIF
037100140120     C*
0372001401201-   C                   ENDIF
037300991214     C*
037400970526     C                   ENDSR
037500970526     C*------------------------------------------------------------------------*
037600991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
037700970526     C*------------------------------------------------------------------------*
037800970526     C     DECODI        BEGSR
037900121019     C*
038000120927     C                   SETOFF                                       505152
038100140109     c                   EVAL      VIDUTI01  = *blank
038200140109     c                   EVAL      VIDUTI02  = *blank
038300140109     c                   EVAL      VIDUTI03  = *blank
038400140109     c                   EVAL      VIDUTI04  = *blank
038500140109     c                   EVAL      VIDUTI05  = *blank
038600140109     c                   EVAL      VIDUTI06  = *blank
038700140109     c                   EVAL      VIDUTI07  = *blank
038800140109     c                   EVAL      VIDUTI08  = *blank
038900140109     c                   EVAL      VIDUTI09  = *blank
039000140109     c                   EVAL      VIDUTI10  = *blank
039100140120     c                   EVAL      VDDPGM01  = *blank
039200140120     c                   EVAL      VDDPGM02  = *blank
039300140120     c                   EVAL      VDDPGM03  = *blank
039400140120     c                   EVAL      VDDPGM04  = *blank
039500140120     c                   EVAL      VDDPGM05  = *blank
039600140120     c                   EVAL      VDDPGM06  = *blank
039700140120     c                   EVAL      VDDPGM07  = *blank
039800140120     c                   EVAL      VIDMSG    = *blank
039900150407     c                   RESET                   wVAB
040000150407     c                   RESET                   wVAO
040100121001     C*
040200140116     C* Lib e File obbligatori
040300121001     C     ERRL00        IFEQ      '0'
040400140116     C                   IF        VIDLib = *blank or VIDFile = *blank
040500120927     C                   MOVEL     '1'           ERRL00
040600130108     C                   SETON                                        51
040700120927     C                   MOVEL     MSG(3)        DSMSMS
040800120927     C                   ENDIF
040900140116     C                   ENDIF
041000150407     C*
041100150407     C* Definisco se è stato richiesto VAB o VAO
041200150407     C                   IF        VIDPosRSD <> 0 or
041300150407     C                             VIDPosIND <> 0 or
041400150407     C                             VIDPosLOD <> 0 or
041500150407     C                             VIDPotNOTE<> 0 or
041600150407     C                             VIDPotNOTI<> 0
041700150407     C                   EVAL      wVAB = *on
041800150407     C                   ENDIF
041900150407     C                   IF        VIDVAORSR <> 0 or
042000150407     C                             VIDVAOTER <> 0 or
042100150407     C                             VIDVAORFA <> 0
042200150407     C                   EVAL      wVAO = *on
042300150407     C                   ENDIF
042400150407     C*
042500150407     C* Se indico posizioni per VAB non posso indicare quelle di VAO (e viceversa)
042600150407     C     ERRL00        IFEQ      '0'
042700150407     C                   IF        wVAB = *on
042800150407     C                              and
042900150407     C                             wVAO = *on
043000150407     C                   MOVEL     '1'           ERRL00
043100150407     C                   MOVEL     MSG(7)        DSMSMS
043200150407     C                   ENDIF
043300150407     C                   ENDIF
043400140116     C*
043500150407     C* Sono obbligatorie queste posizioni per VAB
043600140116     C     ERRL00        IFEQ      '0'
043700150407     C                   IF        (VIDPosRSD = 0 or
043800150407     C                              VIDPosIND = 0 or
043900150407     C                              VIDPosLOD = 0   )
044000150407     C                             and wVAO = *off
044100140116     C                   MOVEL     '1'           ERRL00
044200140116     C                   SETON                                        52
044300140116     C                   MOVEL     MSG(4)        DSMSMS
044400140116     C                   ENDIF
044500140116     C                   ENDIF
044600150407     C*
044700150407     C* Sono obbligatorie queste posizioni per VAO
044800150407     C     ERRL00        IFEQ      '0'
044900150407     C                   IF        VIDVAORSR = 0
045000150407     C                             and wVAB = *off
045100150407     C                   MOVEL     '1'           ERRL00
045200150407     C                   SETON                                        53
045300150407     C                   MOVEL     MSG(5)        DSMSMS
045400150407     C                   ENDIF
045500150407     C                   ENDIF
045600120927     C*
045700970528     C                   ENDSR
045800970526     C*------------------------------------------------------------------------*
045900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
046000970526     C*------------------------------------------------------------------------*
046100970526     C     AZZVID        BEGSR
046200970526     C* CAMPI
046300970526     C*
046400970526     C                   ENDSR
046500970526     C*------------------------------------------------------------------------*
046600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
046700970526     C*------------------------------------------------------------------------*
046800970526     C     IMPFUN        BEGSR
046900970526     C*
047000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
047100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
047200970526     C                   EXSR      FUNGES
047300970526     C*
047400970526     C                   ENDSR
047500020923     C*--------------------------------------------------------------------------------------------*
047600020923     C* REPERISCE I DATI UTENTE
047700020923     C*--------------------------------------------------------------------------------------------*
047800020923     C     REPDATIUTE    BEGSR
047900020923     C*
048000020923     C* INIZIALIZZA VARIABILI DI WRK
048100020923     C                   CLEAR                   TIBS34DS
048200020923     C                   CLEAR                   AZUTEDS
048300020923     C                   CLEAR                   DDATIUTE
048400020923     C*
048500020923     C     *DTAARA       DEFINE    §azute        azuteds
048600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
048700020923     C                   IN(E)     *DTAARA
048800020923if  1C                   IF        %Error
048900020923     c                   EVAL      I34Tla = 'L'
049000020923     C                   CALL      'TIBS34R'
049100020923     C                   PARM                    Tibs34Ds
049200020923     C                   IN        *DTAARA
049300020923e   1C                   ENDIF
049400020923     C*
049500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
049600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
049700020923     C*
049800020923     C                   ENDSR
049900970526     C*------------------------------------------------------------------------*
050000970526     C* *INZSR - OPERAZIONI INIZIALI
050100970526     C*------------------------------------------------------------------------*
050200970526     C     *INZSR        BEGSR
050300991217     C*
050400991217     C     *ENTRY        PLIST
050500991217     C                   PARM                    KPJBA
050600020923     C*
050700020923     C* REPERISCE I DATI DELL'UTENTE
050800020923     C                   EXSR      REPDATIUTE
050900970526     C*
051000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
051100970526     C     XMSGPA        PLIST
051200970526     C                   PARM                    DSMSMS
051300970526     C                   PARM                    DSMSPG
051400970526     C                   PARM                    DSMSKE
051500970526     C                   PARM                    DSMSER
051600970526     C*
051700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
051800970526     C                   CALL      'XKEY'
051900970526     C                   PARM                    DSKEY
052000970526     C*---
052100970526     C* VARIABILI NON RIFERITE AL DATA BASE
052200970526     C*---
052300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
052400991214     C                   MOVEL     *BLANKS       DEPOP1            3
052500970526     C*---
052600970526     C* VARIABILI DI CONTROLLO
052700970526     C*---
052800970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
052900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
053000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
053100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
053200970526     C*
053300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
053400140114     C                   MOVEL     'UBFMTTITR1'  DSMSPG
053500140625     C                   MOVEL     'UBFMTTITR1'  MSGQUE
053600970526     C                   MOVEL     *BLANKS       MSGKEY
053700970526     C                   MOVEL     *BLANKS       DSMSMS
053800970528     C*
053900970528     C* CALCOLA LA DATA CORRENTE
054000970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
054100970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
054200970528     C                   Z-ADD     WN8           G08DAT
054300970528     C                   Z-ADD     *ZEROS        G08INV
054400970528     C                   MOVEL     '0'           G08ERR
054500970528     C                   CALL      'XSRDA8'
054600970528     C                   PARM                    WLBDA8
054700970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
054800000310     C*
054900000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
055000000310     C                   EXSR      AZZVID
055100970526     C*
055200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
055300970526     C                   EXSR      IMPFUN
055400970526     C*
055500970526     C                   ENDSR
055600970526     C*------------------------------------------------------------------------*
055700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
055800970526     C*------------------------------------------------------------------------*
055900970526     C     FUNGES        BEGSR
056000970526     C*
056100970526     C* PULISCO CAMPI DI LAVORO
056200970526     C                   Z-ADD     0             W                 5 0
056300970526     C*
056400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
056500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
056600970526     C     1             DO        24            I
056700970526     C                   MOVEL     FUD(I)        FUDRIG
056800970526     C*
056900970526     C* PULIZIA FUNZIONI PRECEDENTI
057000970526     C                   MOVEL     *BLANKS       FUV(I)
057100970526     C*
057200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
057300970526     C* (SALTA F24)
057400970526     C     FUDATT        IFEQ      'S'
057500970526     C     FUDFUN        ANDNE     '24'
057600970526     C                   ADD       1             W
057700970526     C                   MOVEL     FUDDES        FUV(W)
057800970526     C                   END
057900970526     C                   END                                                    DO WHILE
058000970526     C*
058100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
058200970526     C* ALTRIMENTI DISATTIVO F24
058300970526     C     1             DO        24            I
058400970526     C                   MOVEL     FUD(I)        FUDRIG
058500970526     C*
058600970526     C     FUDFUN        IFEQ      '24'
058700970526     C     W             IFGT      6
058800970526     C                   MOVEL     'S'           FUDATT
058900970526     C                   ELSE
059000970526     C                   MOVEL     *BLANKS       FUDATT
059100970526     C                   END                                                    W GT 8
059200970526     C                   MOVEL     FUDRIG        FUD(I)
059300970526     C                   END
059400970526     C                   END                                                    DO WHILE
059500970526     C*
059600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
059700970526     C     W             DIV       6             I
059800970526     C                   MVR                     Z
059900970526     C*
060000970526     C     Z             IFNE      0
060100970526     C     I             ADD       1             Z
060200970526     C                   ELSE
060300970526     C                   Z-ADD     I             Z                 5 0
060400970526     C                   END
060500970526     C*
060600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
060700970526     C                   ADD       1             FUNGRU
060800970526     C     FUNGRU        IFGT      Z
060900970526     C     FUNGRU        ORLT      1
061000970526     C                   Z-ADD     1             FUNGRU
061100970526     C                   END
061200970526     C*
061300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
061400970526     C     FUNGRU        IFEQ      1
061500970526     C                   MOVEL     FUV(1)        FUN001
061600970526     C                   MOVEL     FUV(2)        FUN002
061700970526     C                   MOVEL     FUV(3)        FUN003
061800970526     C                   MOVEL     FUV(4)        FUN004
061900970526     C                   MOVEL     FUV(5)        FUN005
062000970526     C                   MOVEL     FUV(6)        FUN006
062100970526     C                   END
062200970526     C*
062300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
062400970526     C     FUNGRU        IFEQ      2
062500970526     C                   MOVEL     FUV(7)        FUN001
062600970526     C                   MOVEL     FUV(8)        FUN002
062700970526     C                   MOVEL     FUV(9)        FUN003
062800970526     C                   MOVEL     FUV(10)       FUN004
062900970526     C                   MOVEL     FUV(11)       FUN005
063000970526     C                   MOVEL     FUV(12)       FUN006
063100970526     C                   END
063200970526     C*
063300970526     C* TERZO GRUPPO DI 6 FUNZIONI
063400970526     C     FUNGRU        IFEQ      3
063500970526     C                   MOVEL     FUV(13)       FUN001
063600970526     C                   MOVEL     FUV(14)       FUN002
063700970526     C                   MOVEL     FUV(15)       FUN003
063800970526     C                   MOVEL     FUV(16)       FUN004
063900970526     C                   MOVEL     FUV(17)       FUN005
064000970526     C                   MOVEL     FUV(18)       FUN006
064100970526     C                   END
064200970526     C*
064300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
064400970526     C     FUNGRU        IFEQ      4
064500970526     C                   MOVEL     FUV(19)       FUN001
064600970526     C                   MOVEL     FUV(20)       FUN002
064700970526     C                   MOVEL     FUV(21)       FUN003
064800970526     C                   MOVEL     FUV(22)       FUN004
064900970526     C                   MOVEL     *BLANKS       FUN005
065000970526     C                   MOVEL     *BLANKS       FUN006
065100970526     C                   END
065200970526     C*
065300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
065400970526     C                   MOVEL     *BLANKS       FUN007
065500970526     C                   MOVEL     *BLANKS       FUN008
065600970526     C*
065700970526     C     1             DO        24            I
065800970526     C                   MOVEL     FUD(I)        FUDRIG
065900970526     C     FUDFUN        IFEQ      '24'
066000970526     C     FUDATT        ANDEQ     'S'
066100970526     C                   MOVEL     FUDDES        FUN008
066200970526     C                   END
066300970526     C                   END                                                    DO
066400970526     C*
066500970526     C                   ENDSR
066600970526     C*------------------------------------------------------------------------*
066700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
066800970526     C*------------------------------------------------------------------------*
066900970526     C     FUNCON        BEGSR
067000970526     C*
067100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
067200970526     C                   EXSR      FUNVAR
067300970526     C*
067400970526     C* PULISCO CAMPI DI LAVORO
067500970526     C                   Z-ADD     0             I                 5 0
067600970526     C                   Z-ADD     0             W                 5 0
067700970526     C*
067800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
067900970526     C     FUNKEY        IFNE      *BLANKS
068000970526     C     1             DO        24            I
068100970526     C                   MOVEL     FUD(I)        FUDRIG
068200970526     C*
068300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
068400970526     C     FUNKEY        IFEQ      FUDFUN
068500970526     C     FUDATT        ANDEQ     'S'
068600970526     C                   Z-ADD     I             W
068700970526     C                   END
068800970526     C                   END                                                    DO WHILE
068900970526     C*
069000970526     C* FUNZIONE NON ATTIVA (ERRORE)
069100970526     C     W             IFEQ      0
069200970526     C*
069300970526     C* EMISSIONE MESSAGGIO DI ERRORE
069400970526     C                   MOVEL     MSG(1)        DSMSMS
069500970526     C*
069600970526     C                   END                                                    W=0
069700970526     C                   END                                                    FUNKEY NE *BLANKS
069800970526     C*
069900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
070000970526     C     W             IFNE      0
070100970526     C     FUNKEY        OREQ      *BLANKS
070200970526     C     FUNKEY        IFEQ      *BLANKS
070300970526     C                   EXSR      ENTGES
070400970526     C                   END
070500970526     C     FUNKEY        IFEQ      '03'
070600970526     C                   EXSR      F03GES
070700970526     C                   END
070800970526     C     FUNKEY        IFEQ      '06'
070900970526     C                   EXSR      F06GES
071000970526     C                   END
071100130109     C     FUNKEY        IFEQ      '07'
071200130109     C                   EXSR      F07GES
071300130109     C                   END
071400970526     C     FUNKEY        IFEQ      '12'
071500970526     C                   EXSR      F12GES
071600970526     C                   END
071700970526     C     FUNKEY        IFEQ      '24'
071800970526     C                   EXSR      F24GES
071900970526     C                   END
072000970526     C                   END
072100970526     C*
072200970526     C                   ENDSR
072300970526     C*------------------------------------------------------------------------*
072400970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
072500970526     C*------------------------------------------------------------------------*
072600970526     C     FUNVAR        BEGSR
072700970526     C*
072800970526     C* INIZIALIZZA VARIABILE -FUNKEY-
072900970526     C*
073000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
073100970526     C     KEY           IFEQ      ENTER
073200970526     C                   MOVEL     *BLANKS       FUNKEY
073300970526     C                   END
073400970526     C     KEY           IFEQ      F1
073500970526     C                   MOVEL     '01'          FUNKEY
073600970526     C                   END
073700970526     C     KEY           IFEQ      F2
073800970526     C                   MOVEL     '02'          FUNKEY
073900970526     C                   END
074000970526     C     KEY           IFEQ      F3
074100970526     C                   MOVEL     '03'          FUNKEY
074200970526     C                   END
074300970526     C     KEY           IFEQ      F4
074400970526     C                   MOVEL     '04'          FUNKEY
074500970526     C                   END
074600970526     C     KEY           IFEQ      F5
074700970526     C                   MOVEL     '05'          FUNKEY
074800970526     C                   END
074900970526     C     KEY           IFEQ      F6
075000970526     C                   MOVEL     '06'          FUNKEY
075100970526     C                   END
075200970526     C     KEY           IFEQ      F7
075300970526     C                   MOVEL     '07'          FUNKEY
075400970526     C                   END
075500970526     C     KEY           IFEQ      F8
075600970526     C                   MOVEL     '08'          FUNKEY
075700970526     C                   END
075800970526     C     KEY           IFEQ      F9
075900970526     C                   MOVEL     '09'          FUNKEY
076000970526     C                   END
076100970526     C     KEY           IFEQ      F10
076200970526     C                   MOVEL     '10'          FUNKEY
076300970526     C                   END
076400970526     C     KEY           IFEQ      F11
076500970526     C                   MOVEL     '11'          FUNKEY
076600970526     C                   END
076700970526     C     KEY           IFEQ      F12
076800970526     C                   MOVEL     '12'          FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F13
077100970526     C                   MOVEL     '13'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F14
077400970526     C                   MOVEL     '14'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F15
077700970526     C                   MOVEL     '15'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F16
078000970526     C                   MOVEL     '16'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F17
078300970526     C                   MOVEL     '17'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F18
078600970526     C                   MOVEL     '18'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F19
078900970526     C                   MOVEL     '19'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F20
079200970526     C                   MOVEL     '20'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F21
079500970526     C                   MOVEL     '21'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F22
079800970526     C                   MOVEL     '22'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F23
080100970526     C                   MOVEL     '23'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      F24
080400970526     C                   MOVEL     '24'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      ROLLUP
080700970526     C                   MOVEL     'RU'          FUNKEY
080800970526     C                   END
080900970526     C     KEY           IFEQ      HELP
081000970526     C                   MOVEL     'HP'          FUNKEY
081100970526     C                   END
081200970526     C                   ENDSR
081300970526     C*------------------------------------------------------------------------*
081400970526     C* NOTE COSTRUZIONE SCHIERE
081500970526     C*------------------------------------------------------------------------*
081600970526     C*
081700970526     C* -MSG-MESSAGGI
081800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
081900970526     C* (NON TOCCARE MSG,1 E MSG,2)
082000970526     C*
082100970526     C* -CMD-COMANDI
082200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
082300970526     C*
082400970526     C* -OPD-OPZIONI
082500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
082600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
082700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
082800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
082900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
083000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
083100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
083200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
083300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
083400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
083500970526     C*
083600970526     C* -FUD-FUNZIONI
083700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
083800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
083900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
084000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
084100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
084200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
084300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
084400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
084500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
084600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
084700140818     Oqsysprt   E            testdett          2
084800140818     O                       KNSIF
084900140818     O                       SDSUSER             +2
085000140818     O                                           67 '* Diagnostica posizioni ca'
085100140818     O                                           +0 'mpi chiave *'
085200140818     O                       G08DAT        Y    130
085300140818     O          E            testdett    1
085400140818     O                       SDSPGM
085500140818     O                       ORA                130
085600140818     O          E            testdett    1
085700140818     O                                              'Lib/File/Membro:'
085800140818     O                       VIDLIB              +2
085900140818     O                                           +2 '/'
086000140818     O                       VIDFILE             +2
086100140818     O                                           +2 '/'
086200140818     O                       VIDMBR              +2
086300140818     O          E            testdett    1
086400140818     O                                              'Posizioni'
086500140818     O                                           +2 'VABRSD:'
086600140818     O                       VIDPOSRSD           +2
086700140818     O                                           +3 'VABIND:'
086800140818     O                       VIDPOSIND           +2
086900140818     O                                           +3 'VABLOD:'
087000140818     O                       VIDPOSLOD           +2
087100140818     O                                           +3 'VATNOT_E:'
087200140818     O                       VIDPOTNOTE          +2
087300140818     O                                           +3 'VATNOT_I:'
087400140818     O                       VIDPOTNOTI          +2
087500140818     O          E            testdett    1
087600140818     O                                              'Sottogruppo per Testo:'
087700140818     O                       VIDSELTXT           +2
087800140818     O          E            testdett    2
087900140818     O                                           +1 'Nome pgm  '
088000140818     O                                           +2 'Descrizione'
088100140818     O          E            dettaglio   1
088200140818     O                       TIDNome             +1
088300140818     O                       TIDDescr            +2
088400970526** MSG - MESSAGGI
088500120927Tasto funzionale non gestito                                                   1
088600120927Numero chiamate obbligatorio                                                   2
088700140116Libreria e file sorgente obbligatori                                           3
088800150407Per VAB sono obbligatorie le posizioni di RSD-IND-LOD                          4
088900150407Per VAO sono obbligatorie le posizioni di RSR                                  5
089000130108Campo obbligatorio                                                             6
089100150407O indico piosizioni per VAB oppure per VAO, non entrambe!                      7
089200130108                                                                               8
089300120927                                                                               9
089400120927                                                                               10
089500120927                                                                               11
089600120927                                                                               12
089700120927                                                                               13
089800120927Data errata                                                                    14
089900140116                                                                               15
090000120927                                                                               16
090100120927                                                                               17
090200120927                                                                               18
090300120927                                                                               19
090400140116Diagnostica conclusa - controllare l'esito                                     20
090500120927Scelta obbligatoria                                                            21
090600130110Eseguita la stampa                                                             22
090700121108                                                                               23
090800121108                                                                               24
090900970526** FUD - FUNZIONI
091000970526                   -        (S/N)=N                        funz.01¬
091100970526                   -        (S/N)=N                        funz.02¬
091200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
091300970526                   -        (S/N)=N                        funz.04¬
091400970526                   -        (S/N)=N                        funz.05¬
091500991214F06=Conferma       -        (S/N)=S                        funz.06¬
091600140818F07=Conf. e Stampa -        (S/N)=S                        funz.07¬
091700970527                   -        (S/N)=N                        funz.08¬
091800970527                   -        (S/N)=N                        funz.09¬
091900970527                   -        (S/N)=N                        funz.10¬
092000970526                   -        (S/N)=N                        funz.11¬
092100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
092200970526                   -        (S/N)=N                        funz.13¬
092300970526                   -        (S/N)=N                        funz.14¬
092400970526                   -        (S/N)=N                        funz.15¬
092500970526                   -        (S/N)=N                        funz.16¬
092600970526                   -        (S/N)=N                        funz.17¬
092700970526                   -        (S/N)=N                        funz.18¬
092800970526                   -        (S/N)=N                        funz.19¬
092900970526                   -        (S/N)=N                        funz.20¬
093000970526                   -        (S/N)=N                        funz.21¬
093100970526                   -        (S/N)=N                        funz.22¬
093200970526                   -        (S/N)=N                        funz.23¬
093300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
