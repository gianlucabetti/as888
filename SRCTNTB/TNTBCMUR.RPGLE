000100010411      /TITLE Manutenzione TNTBE00F: tabella "CMU"
000200000607      *
000300000606     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000400000607      *
000500010411     FTntBcmuD  CF   E             WORKSTN
000600000612     F                                     SFILE(S01:RRNS01)
000700010427     FTnTbe00F  UF A E             DISK
000800010430     F
000900010427     FTnTbe01L  IF   E           K DISK
001000010427     F                                     RENAME(TnTbe000:TnTbe01)
001100010430     F                                     INFDS(InfTnTbe01)
001200010503      *Costanti di servizio.
001300000607     D Errore          C                   '1'
001400000607     D Eseguito        C                   '0'
001500010503
001600010503      *Campi di servizio.
001700010427     D Len             S              5U 0
001800010427     D Blanks          S
001900010427     D                                     LIKE(R01DescOpz)
002000010503      *Nome PGM
002100000628     D Psds           SDS
002200000628     D  PgmName          *PROC
002300010503
002400010503      *Passaggio parametri
002500010503     D KPJBA         E DS
002600010503
002700010503      *DS di servizio.
002800010430     D InfTnTbe01      DS
002900010430     D  RrnTnTbe01           397    400I 0
003000010430     D  WrkKey         S              1
003100010430     D  WrkEofS01      S              1
003200010430     D  WrkCarS01      S              1
003300010430     D  SavOpzione     S
003400000703     D                                     LIKE(S1Opzione)
003500010502     D SavRcdNbr       S
003600010502     D                                     LIKE(C01RcdNbr)
003700010503
003800010503      *Parametri di servizio.
003900000712     D $VIDEO          S             10
004000000608     D RrnS01          S              4  0
004100000616     D Comando         S              1
004200010503
004300010503      *DS Parametri Fuzionali.
004400010503     D TnTbcMuS      E DS
004500010503     D                                     INZ
004600010510     D DCMU          E DS
004700010510     D                                     INZ
004800010503      *DS Dati societari.
004900000607     D XSoc001DS     E DS
005000000607     D                                     INZ
005100000607     D TipXsc          S              6
005200000607     D SocXsc          S              3
005300000607     D CdsXsc          S              9  0
005400000607     D ModXsc          S              3
005500000607     D RtnXsc          S              1
005600010503
005700010503      *Reperimento descrizione oggetto
005800010427     D objd01          s            180
005900010427     D objd02          s              8b 0 inz(%size(objd01))
006000010427     D objd03          s              8    inz('OBJD0200')
006100010427     D objd04          ds
006200010427     D  objd04a                      10
006300010427     D  objd04b                      10
006400010427     D objd05          s             10
006500010427     D/COPY QSYSINC/QRPGLESRC,QUSEC
006600010427     D/COPY QSYSINC/QRPGLESRC,QUSROBJD
006700010423      *--------------------------------------------------------------------*
006800010426      *                       M A I N   L I N E                            *
006900010423      *--------------------------------------------------------------------*
007000010423
007100010423      *Elaborazione parametri ricevuti.
007200010423     C                   EXSR      WrkParm
007300010423
007400010430      *Elaborazione procedure pgm.
007500010426     C                   EXSR      WrkC01
007600010427
007700010430      *Uscita dal pgm.
007800010427     C                   EXSR      Uscita
007900010423      *--------------------------------------------------------------------*
008000010426      *                          E N D   P G M                             *
008100010423      *--------------------------------------------------------------------*
008200010423
008300010423
008400010423
008500010426      *--------------------------------------------------------------------*
008600010426      *                      S U B - R O U T I N E S                       *
008700010426      *--------------------------------------------------------------------*
008800010423
008900010430      *-Controllo ed elaborazione parametri ricevuti-*
009000010430     C     WrkParm       BEGSR
009100010430      *-----------------------
009200010503      *Valorizzo la Ds dei parametri funzionali con
009300010503      *quella di passaggio parametri.
009400010503     C                   EVAL      TnTbcMuS = KpjBu
009500010503      *Testo la funzione richiesta dal pgm chiamante.
009600010430     C                   SELECT
009700010503      *Ricerca e Scelta
009800010503     C                   WHEN      TbcFun = '1'
009900010503     C                   EVAL      *IN80  =  *ON
010000010503      *Manutenzione
010100010503     C                   WHEN      TbcFun = ' '
010200010430     C                   EVAL      *IN80 =  *OFF
010300010503      *altrimenti
010400010503     C                   OTHER
010500010503     C                   EVAL      TbcEsito = Errore
010600010503     C                   EXSR      Uscita
010700010503      *
010800010430     C                   ENDSL
010900010430      *
011000010430     C                   ENDSR
011100010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
011200010430
011300010430      *-Gestione Sub-File-*
011400010430     C     WrkC01        BEGSR
011500010430      *-----------------------
011600010430      *Imposto variabili decisionali di caricamento subfile ed elaborazione
011700010430      *record formato video.
011800010430     C                   EVAL      WrkCarS01 = *ON
011900010430     C                   EVAL      $Video = 'C01'
012000010430      *Inizio elaborazione sub-file
012100010430     C                   DOU       $Video <> 'C01'
012200010430      *Caricamento sub-file
012300010430     C                   EXSR      CarS01
012400010502      * In C01CSRRRN c'è il numero di riga del subfile
012500010502      * su cui era il cursore quando l'utente ha premuto un tasto.
012600010502      * Impostando C01RCDNBR,
012700010502      * visualizzo la stessa pagina che vedeva l'utente
012800010502      * quando ha premuto l'ultimo tasto.
012900010502     C                   IF        C01CsrRrn > *ZERO
013000010502     C                   EVAL      C01RcdNbr = C01CsrRrn
013100010502     C                   ELSE
013200010502     C                   EVAL      C01RcdNbr = SavRcdNbr
013300010502     C                   ENDIF
013400010502      * Se non so quale pagina visualizzare, forzo pagina 1.
013500010502     C                   IF        C01RcdNbr < 1
013600010503     C*$$mm                        OR
013700010503     C*$$mm                        C01RcdNbr > RrnS01
013800010502     C                   EVAL      C01RcdNbr = 1
013900010502     C                   ENDIF
014000010502      * Salvo il record number del subfile.
014100010502     C                   EVAL      SavRcdNbr = C01RcdNbr
014200010430      *Emissione a video del sub-file
014300010430     C                   WRITE     PIEDE
014400010430     C                   WRITE     TESTATA
014500010430     C                   EXFMT     C01
014600010430      *Controllo uso tasti funzionali in Sub-file
014700010430     C                   SELECT
014800010430      * F3  (Fine)
014900010430     C                   WHEN      *INKC=*ON
015000010430     C                   EVAL      $VIDEO=*BLANKS
015100010503     C                   EVAL      TBCRicez = 'C'
015200010503     C                   EXSR      Uscita
015300010430      * F5  (Refresh)
015400010430     C                   WHEN      *INKE=*ON
015500010430     C                   EVAL      WrkCarS01 = *ON
015600010430      * F10 (Immissione)
015700010430     C                   WHEN      *INKJ=*ON
015800010430     C                   EVAL      $VIDEO='R01'
015900010503     C                   CLEAR                   TnTbcmuS
016000010503     C                   EVAL      TBCComand = 'J'
016100010430     C                   EXSR      WrkR01
016200010502      * F13 (Ripetizione)
016300010430     C                   WHEN      *INKM
016400010430     C                   EXSR      RipetiOpz
016500010430      *in tutti gli altri casi
016600010430     C                   OTHER
016700010502      *Controllo ed elaborazione scelte su sub-file.
016800010430     C                   EXSR      GestioneSFL
016900010430     C                   ENDSL
017000010430      *Fine elaborazione sub-file.
017100010430     C                   ENDDO
017200010430      *
017300010430     C                   ENDSR
017400010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
017500010430
017600010430      *-Caricamento Subfile-*
017700010430     C     CarS01        BEGSR
017800010430      *-----------------------
017900010430      *Se è stato richiesto il caricamento del sub-file
018000010430     C                   IF        WrkCarS01 = *ON
018100010430      *Inizializzo sub-file (clear)
018200010430     C                   EVAL      RRNS01 = 0
018300010430     C                   EVAL      *IN90 = *ON
018400010430     C                   WRITE     C01
018500010430     C                   EVAL      *IN90 = *OFF
018600010430     C                   EVAL      *IN91 = *OFF
018700010430     C                   EVAL      *IN92 = *OFF
018800010509     C                   EVAL      *IN93 = *OFF
018900010430      *Imposto la chiave di posizionamento e lettura file
019000010430     C                   EVAL      TbeCod = 'CMU'
019100010430     C                   EVAL      TbeKe1 = C01SetLL
019200010430      *Se scelto posizionamento
019300010430     C                   IF        C01SETLL <> *BLANKS
019400010430     C                   EVAL      WRKKEY = '2'
019500010430      *altrimenti
019600010430     C                   ELSE
019700010430     C                   EVAL      WRKKEY = '1'
019800010430     C                   ENDIF
019900010430      *Posizionamento file
020000010430     C                   EXSR      SetllTbe01
020100010430      *Fino a che no fine file
020200010430     C                   DOU       %EOF(TnTbe01L)
020300010503      *Leggo file
020400010430     C     K01Tbe01      READE     TnTbe01L
020500010503      *SE no fine file
020600010503     C                   IF            NOT %EOF(TnTbe01L)
020700010503      *SE in "ricerca/scelta" non carico i records annullati
020800010503     C                   IF            (*IN80 = *ON
020900010503     C                             AND  TbeATB = *BLANKS)
021000010503     C                              OR  *IN80  = *OFF
021100010430      *Imposto e scrivo sub-file
021200010430     C                   CLEAR                   S1Opzione
021300010430     C                   EXSR      WrtS01
021400010430     C                   ENDIF
021500010430      *
021600010503     C                   ENDIF
021700010503      *
021800010430     C                   ENDDO
021900010430      *Fine se richiesto caricamento sub-file
022000010430     C                   ENDIF
022100010430      *Se scritta almeno una riga
022200010430     C                   IF        RRNS01 > 0
022300010430      *Accendo indicatore di SFLEND
022400010430     C                   EVAL      *IN91 = *ON
022500010430     C                   ENDIF
022600010509      *Se non scritta almeno una riga
022700010509     C                   IF        RRNS01 = 0
022800010509      *Accendo indicatore di no-SFLDSP
022900010509     C                   EVAL      *IN93 = *ON
023000010509     C                   ENDIF
023100010430      *Disattivo opzione di caricamento sub-file
023200010430     C                   EVAL      WrkCarS01 = *OFF
023300010430      *
023400010430     C                   ENDSR
023500010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
023600010430
023700010430      *-Gestione formato video R01-*
023800010430     C     WrkR01        BEGSR
023900010430      *-----------------------
024000010430      *Imposto il video a seconda della funzione richiesta.
024100010430     C                   EXSR      SetVideo
024200010502      *Imposto variabile decisionale
024300010502     C                   EVAL      $Video = 'R01'
024400010430      *Fino a che la variabile decisionale resta settata sul record 'R01'
024500010430     C                   DOU       $Video <> 'R01'
024600010430      *In caso di errori.
024700010430     C                   IF        *IN99
024800010430     C                   EVAL      *IN99 = *OFF
024900010430      *Visualizzo dati di output.
025000010430     C                   WRITE     R01
025100010430     C                   EVAL      *IN99 = *ON
025200010430     C                   ENDIF
025300010502      *Se immessa opzione di 'scelta'
025400010510     C                   IF        TBCOpzio  = '01'
025500010510     C                   EVAL      TBCKe1    = S01TbeKe1
025600010510     C                   EVAL      TBCUni    = S01TbeUni
025700010510     C                   EVAL      TBCUbicaz = S01Ubicaz
025800010502     C                   EXSR      Uscita
025900010502     C                   ENDIF
026000010502      *Se immessa opzione di 'cancellazione/ripristino'
026100010503     C                   IF        TBCOpzio = '04'
026200010430     C                             OR
026300010430      *oppure 'visualizzazione'
026400010503     C                             TBCOpzio = '05'
026500010430     C                   WRITE     R01
026600010502      *impedisco l'immissione a video
026700010430     C                   EXFMT     R01Protect
026800010502     C                   ENDIF
026900010502      *Se immessa opzione di 'modifica'
027000010503     C                   IF        TBCOpzio = '02'
027100010502     C                             OR
027200010502      *oppure 'copia'
027300010503     C                             TBCOpzio = '03'
027400010502     C                             OR
027500010502      *oppure richiesta funzione di immissione
027600010503     C                             TBCComand = 'J'
027700010430      *emissione libera formato video
027800010430     C                   EXFMT     R01
027900010430     C                   ENDIF
028000010430      *Reset indicatore generico di errore a video
028100010430     C                   EVAL      *IN99 = *OFF
028200010430
028300010430      *Testo quale tasto funzionale è stato premuto
028400010430     C                   SELECT
028500010430      * F2 (Decodifica)
028600010430     C                   WHEN      *INKB
028700010430     C                   EXSR      ChkR01
028800010430      * F3 (Fine)
028900010430     C                   WHEN      *INKC
029000010503     C                   EVAL      TBCRicez = 'C'
029100010502     C                   EVAL      $Video = *BLANK
029200011122     C                   UNLOCK    TnTbe00F
029300010430     C                   EXSR      Uscita
029400010430      * F5 (Ripristino)
029500010430     C                   WHEN      *INKE
029600010430     C                   EXSR      SetVideo
029700010430      * F6 (Conferma)
029800010430     C                   WHEN      *INKF
029900010503     C                   EVAL      TBCRicez = 'F'
030000010430      *Controllo e decodifica.
030100010430     C                   EXSR      ChkR01
030200010430      *Se richiesta funzione di conferma su opzione di annullam./ripristino
030300010503     C                   IF            TBCOpzio = '04'
030400010430     C                   SELECT
030500010430      *annullamento
030600010430     C                   WHEN      TbeAtb = *BLANK
030700010430     C                   EVAL      TbeAtb = '1'
030800010430      *ripristino
030900010430     C                   WHEN      TbeAtb = '1'
031000010430     C                   EVAL      TbeAtb = *BLANK
031100010430     C                   ENDSL
031200010430     C                   ENDIF
031300010430      *Se non riscontrati errori aggiorno il record corrente.
031400010430     C                   IF        NOT *IN99
031500010430     C                   EXSR      Aggiorna
031600010508     C                   IF        NOT *IN99
031700010509     C                   IF        TBCComand = 'J'
031800010509     C                   EVAL      $VIDEO    = 'R01'
031900010509     C                   EXSR      SetVideo
032000010509     C                   ELSE
032100010509     C                   EVAL      $VIDEO = 'C01'
032200010430     C                   ENDIF
032300010509     C                   ENDIF
032400010508     C                   ENDIF
032500010430      * F8 (record successivo)
032600010430     C                   WHEN      *INKH
032700010430     C                   CLEAR                   S1Opzione
032800010430     C                   EVAL      WrkCarS01 = *OFF
032900010430     C                   EVAL      $Video = 'C01'
033000010503     C                   EVAL      TBCRicez = 'H'
033100010430      * F12 (annullo)
033200010430     C                   WHEN      *INKL
033300010503     C                   EVAL      TBCRicez = 'L'
033400010430     C                   CLEAR                   S1Opzione
033500011122     C                   UNLOCK    TnTbe00F
033600010509      *non eseguo il ricaricamento del sub-file se no ritorno da F10
033700010509     C                   IF        TBCComand = 'J'
033800010509     C                   EVAL      WrkCarS01 = *ON
033900010509     C                   ELSE
034000010430     C                   EVAL      WrkCarS01 = *OFF
034100010509     C                   ENDIF
034200010430     C                   EVAL      $Video = 'C01'
034300010430      * Invio
034400010430     C                   OTHER
034500010430     C                   EXSR      ChkR01
034600010430      *
034700010430     C                   ENDSL
034800010502      *
034900010430      *Fine mentre il record da gestire è il 'R01'.
035000010430     C                   ENDDO
035100010430      *
035200010430     C                   ENDSR
035300010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
035400010430
035500010430      *- Ripetizione opzione in tutte le righe del subfile.-*
035600010430     C     RipetiOpz     BEGSR
035700010430      *-----------------------
035800010430     C                   IF        C01CsrRrn > *ZERO
035900010430      *
036000010430     C                   EVAL      RrnS01 = C01CsrRrn
036100010430     C     RrnS01        CHAIN     S01
036200010430     C                   IF        %FOUND
036300010430     C                             AND
036400010430     C                             S1Opzione > *ZERO
036500010430      *
036600010430     C                   EVAL      SavOpzione = S1Opzione
036700010430     C                   EVAL      *IN92 = *ON
036800010430     C                   UPDATE    S01
036900010430      *
037000010430     C                   EVAL      WrkEofS01 = *OFF
037100010430     C                   DOU       WrkEofS01 = *ON
037200010430     C                   ADD       1             RrnS01
037300010430     C     RrnS01        CHAIN     S01
037400010430     C                   IF        %FOUND
037500010430     C                   EVAL      S1Opzione = SavOpzione
037600010430     C                   UPDATE    S01
037700010430     C                   ELSE
037800010430     C                   EVAL      WrkEofS01 = *ON
037900010430     C                   ENDIF
038000010430     C                   ENDDO
038100010430      *
038200010430     C                   EVAL      *IN92  = *OFF
038300010430      *
038400010430     C                   ENDIF
038500010430      *
038600010430     C                   ENDIF
038700010430      *
038800010430     C                   ENDSR
038900010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
039000010430
039100010430      *-Imposto record R01 a seconda della funzione chiamante.
039200010430     C     SetVideo      BEGSR
039300010430      *-----------------------
039400010430      *Ripulisco il formato video R01
039500010430     C                   EXSR      Pulire
039600010430      *Controllo quale funzione ha richiesto l'utilizzo del formato R01
039700010430     C                   SELECT
039800010430      * F10 (Immissione)
039900010503     C                   WHEN      TBCComand = 'J'
040000010430     C                   EVAL      R01DescOpz = 'Immissione'
040100010430      * Opzione "02" (modifica)
040200010503     C                   WHEN      TBCOpzio = '02'
040300010430     C                   EVAL      R01DescOpz = 'Modifica'
040400010430     C                   EXSR      Imposta
040500010430      * Opzione "03" (copia)
040600010503     C                   WHEN      TBCOpzio ='03'
040700010430     C                   EVAL      R01DescOpz = 'Copia'
040800010430     C                   EXSR      Imposta
040900010430      * Opzione "04" (cancellazione/ripristino)
041000010503     C                   WHEN          TBCOpzio ='04'
041100010430     C                   EXSR      Imposta
041200010430      *Se richiesta 'cancellazione'.
041300010430     C                   IF        TbeAtb = *BLANKS
041400010531     C                   EVAL      R01DescOpz = 'Annullamento'
041500010430     C                   ENDIF
041600010430      *Se richiesto 'ripristino'.
041700010430     C                   IF        TbeAtb = '1'
041800010430     C                   EVAL      R01DescOpz = 'Ripristino'
041900010430     C                   ENDIF
042000010430      * Opzione "05" (visualizzazione)
042100010503     C                   WHEN      TBCOpzio ='05'
042200010430     C                   EVAL      R01DescOpz = 'Visualizzazione'
042300010430     C                   EXSR      Imposta
042400010430      *Fine scelta.
042500010430     C                   ENDSL
042600010430      *Centro la descrizione della funzione nel formato video.
042700010430     C                   EVAL      Len = (%LEN(R01DescOpz) -
042800010430     C                             %LEN(%TRIMR(R01DescOpz))) / 2
042900010430     C                   EVAL      R01DescOpz
043000010430     C                             =
043100010430     C                             %SUBST(Blanks:1:Len)
043200010430     C                             +
043300010430     C                             %TRIMR(R01DescOpz)
043400010430      *
043500010430     C                   ENDSR
043600010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
043700010430
043800010430      *- Controlli video R01 -*
043900010430     C     ChkR01        BEGSR
044000010430      *-----------------------
044100010430     C                   EVAL      *IN99 = *OFF
044200010430      *
044300010430     C                   CLEAR                   R01DescUte
044400010510     C                   CLEAR                   R01DesUbi
044500010430
044600010430      * CODICE UTENTE POSTA
044700010430     C                   IF        R01TBEKe1 = *BLANKS
044800010430     C                   EVAL      *IN99 = *ON
044900010430     C                   EVAL      *IN98 = *ON
045000010430     C                   ENDIF
045100010430      * DECODIFICA UTENTE POSTA
045200010430     C                   IF            R01TBEKe1 <> *BLANKS
045300010430     C
045400010430      *Reperisco la descrizione dell'oggetto
045500010430     C                   eval      objd04a = R01TbeKe1
045600010430     C                   eval      objd04b = 'QSYS'
045700010430     C                   eval      objd05  = '*USRPRF'
045800010430     C                   reset                   qusec
045900010430     C                   eval      qusbprv = %size(qusec)
046000010430     C                   call      'QUSROBJD'
046100010430     C     qusd0200      parm      *blanks       objd01
046200010430     C                   parm                    objd02
046300010430     C                   parm                    objd03
046400010430     C                   parm                    objd04
046500010430     C                   parm                    objd05
046600010430     C                   parm                    qusec
046700010430
046800010430      *Se il profilo utente immesso esiste eseguo la decodifica a video
046900010430     C                   IF        Qusei = *BLanks
047000010430     C                   EVAL      R01DescUte = qustd12
047100011015     C                   IF        R01TbeUni  = *BLANK
047200010430     C                   EVAL      R01TbeUni  = qustd12
047300011015     C                   ENDIF
047400010430     C                   ENDIF
047500010430     C                   ENDIF
047600010430      * DESCRIZIONE UTENTE POSTA
047700010430     C                   IF        R01TBEUni = *BLANK
047800010430     C                   EVAL      *IN99 = *ON
047900010430     C                   EVAL      *IN97 = *ON
048000010430     C                   ENDIF
048100010510      * UBICAZIONE
048200010510     C                   SELECT
048300010510
048400010510     C                   WHEN      R01Ubicaz = '1'
048500011019     C                   EVAL      R01DesUbi = 'Filiale I livello'
048600010510
048700010510     C                   WHEN      R01Ubicaz = '2'
048800011019     C                   EVAL      R01DesUbi = 'Filiale II livello'
048900010510
049000010510     C                   WHEN      R01Ubicaz = 'S'
049100010510     C                   EVAL      R01DesUbi = 'Sede'
049200011019
049300011019     C                   WHEN      R01Ubicaz = 'F'
049400011019     C                   EVAL      R01DesUbi = 'Filiali'
049500010510
049600010510     C                   OTHER
049700010510     C                   CLEAR                   R01Ubicaz
049800010510     C                   EVAL      R01DesUbi = 'Ovunque'
049900010510      *
050000010510     C                   ENDSL
050100010510      *
050200010430     C                   ENDSR
050300010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
050400010430
050500010430      *-Aggiornamento record R01-*
050600010430     C     Aggiorna      BEGSR
050700010430      *-----------------------
050800010430      *Imposto campi del data base
050900010510     C                   EVAL      TbeKe1     = R01TbeKe1
051000010510      *
051100010510     C                   EVAL      §CMUDesc   = R01TbeUni
051200010510     C                   EVAL      §CMUUbicaz = R01Ubicaz
051300010510      *
051400010510     C                   EVAL      TbeUni = DCMU
051500011122     C                   CLEAR                   TbeFTr
051600011122     C                   CLEAR                   TbeDTr
051700010430      *Testo quale funzione o opzione ha richiesto l'aggiornamento record
051800010430     C                   SELECT
051900010430      * F10 (immissione) - Opzione "03" (copia)
052000010503     C                   WHEN      TBCComand = 'J'
052100010430     C                             OR
052200010503     C                             TBCOpzio = '03'
052300010430      *Scrivo nuovo record con gestione errore per chiave duplicata
052400010430     C                   WRITE(E)  TnTbe000
052500010430     C                   EXSR      ErrDupKey
052600010430      *Opzione "02" (modifica)
052700010503     C                   WHEN      TBCOpzio = '02'
052800010430      *Update record e controllo errore per chiave duplicata
052900010430     C                   UPDATE(E) TnTbe000
053000010430     C                   EXSR      ErrDupKey
053100010430      *Opzione "04" (cancellazione/ripristino)
053200010503     C                   WHEN      TBCOpzio = '04'
053300010430     C                   UPDATE    TnTbe000
053400010430
053500010430     C                   ENDSL
053600010430      *
053700010430     C                   ENDSR
053800010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
053900010430
054000010430      *-Pulizia formato video R01-*
054100010430     C     Pulire        BEGSR
054200010430      *-----------------------
054300010430     C                   CLEAR                   R01DescOpz
054400010430     C                   CLEAR                   R01TbeKe1
054500010430     C                   CLEAR                   R01TbeUni
054600010430     C                   CLEAR                   R01DescUte
054700010510     C                   CLEAR                   R01Ubicaz
054800010510     C                   CLEAR                   R01DesUbi
054900010430      *
055000010430     C                   ENDSR
055100010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
055200010430
055300010430      *-Imposto il formato video R01-*
055400010430     C     Imposta       BEGSR
055500010430      *-----------------------
055600010430      *Imposto Numero Relativo di Record
055700010503     C                   EVAL      TBCRelRec = S01RrnTbe
055800010430      *Reperisco il record sul file
055900010503     C     TBCRelRec     CHAIN     TnTbe00F
056000010430      *Imposto i campi a video
056100010531     C                   EVAL      DCMU = TbeUni
056200010430     C                   EVAL      R01TbeKe1  = TbeKe1
056300010430     C                   EVAL      R01TbeUni  = TbeUni
056400010531     C                   EVAL      R01Ubicaz  = §CMUUbicaz
056500010430      *
056600010430     C                   ENDSR
056700010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
056800010430
056900010430      *-Gestione formato video Sub-file-*
057000010430     C     GestioneSFL   BEGSR
057100010430      *-----------------------
057200010502      *Set flag
057300010430     C                   EVAL      WrkEofS01 = *OFF
057400010502      *Inizio lettura sub-file
057500010430     C                   DOW       WrkEofS01 = *OFF
057600010430     C                             AND
057700010430     C                             *IN91
057800010502      *Intercetto immissione opzione
057900010430     C                   READC     S01
058000010502      *Se fine sub-file
058100010430     C                   IF        %EOF
058200010430     C                   EVAL      WrkCarS01 = *ON
058300010430     C                   LEAVE
058400010430     C                   ENDIF
058500010502      *Se è stata immesso un valore di opzione
058600010430     C                   IF        S1Opzione <> 0
058700010502      *Reset Ds di servizio
058800010503     C                   CLEAR                   TnTbcMuS
058900010502      *Controllo ed elaborazione opzione immessa
059000010430     C                   SELECT
059100010502      *Opzione "01" (scelta)
059200010502     C                   WHEN          S1Opzione = 1
059300010502     C                             AND *IN80 = *ON
059400010503     C                   EVAL      TBCOpzio = '01'
059500010502      *Opzione "02" (modifica)
059600010502     C                   WHEN          S1Opzione = 2
059700010502     C                             AND *IN80 = *OFF
059800010503     C                             AND S01TbeATB= *BLANK
059900010503     C                   EVAL      TBCOpzio = '02'
060000010502      *Opzione "03" (copia)
060100010502     C                   WHEN          S1Opzione = 3
060200010502     C                             AND *IN80 = *OFF
060300010503     C                             AND S01TbeATB= *BLANK
060400010503     C                   EVAL      TBCOpzio = '03'
060500010502      *Opzione "04" (annullo/ripristino)
060600010502     C                   WHEN          S1Opzione = 4
060700010502     C                             AND *IN80 = *OFF
060800010503     C                   EVAL      TBCOpzio = '04'
060900010502      *Opzione "05" (visualizzazione)
061000010502     C                   WHEN          S1Opzione = 5
061100010503     C                   EVAL      TBCOpzio = '05'
061200010430     C                   ENDSL
061300010503
061400010503      *Se immessa almeno un opzione valida
061500010503     C                   IF        TBCOpzio <> *BLANKS
061600010503
061700010503
061800010502      *Routine per la gstione del formato video R01.
061900010430     C                   EXSR      WrkR01
062000010502      * Se la routine chiamata si è chiusa con ...
062100010430     C                   SELECT
062200010430      * F6
062300010503     C                   WHEN      TBCRicez = 'F'
062400010430     C                   CLEAR                   S1Opzione
062500010430     C                   EVAL      WrkCarS01 = *ON
062600010430      * F12
062700010503     C                   WHEN      TBCRicez = 'L'
062800010430     C                   CLEAR                   S1Opzione
062900010430     C                   EVAL      WrkEofS01 = *ON
063000010430      * altrimenti
063100010430     C                   OTHER
063200010430     C                   EVAL      *IN92 = *ON
063300010430     C                   ENDSL
063400010502      *
063500010430     C                   UPDATE    S01
063600010430     C                   EVAL      *IN92 = *OFF
063700010503      *Fine SE immesso almeno un valore valido
063800010430     C                   ENDIF
063900010503      *Fine SE immesso un valore di opzione
064000010503     C                   ENDIF
064100010430      *
064200010430     C                   ENDDO
064300010430      *
064400010430     C                   ENDSR
064500010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
064600010430
064700010430      *-Posizionamento File-*
064800010430     C     SetllTbe01    BEGSR
064900010430      *-----------------------
065000010430     C                   SELECT
065100010430      *Chiave totale.
065200010430     C                   WHEN      WRKKEY = '1'
065300010430     C     K01Tbe01      SETLL     TnTbe01l
065400010430      *Chiave parziale.
065500010430     C                   WHEN      WRKKEY = '2'
065600010430     C     K02Tbe01      SETLL     TnTbe01l
065700010430      *
065800010430     C                   ENDSL
065900010430      *
066000010430     C                   ENDSR
066100010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
066200010430
066300010430      *- Scrivo subfile -*
066400010430     C     WrtS01        BEGSR
066500010430      *-----------------------
066600010430      *Se non raggiunto limite massimo di caricamento
066700010430     C                   IF        RrnS01 < 9999
066800010430      *Imposto campi di sub-file
066900010430     C                   EXSR      SetS01
067000010430      *Imposto numeratore righe e numero relativo di record
067100010430     C                   EVAL      RRNS01 = RRNS01 + 1
067200010430     C                   EVAL      S01RrnTbe = RrnTnTbe01
067300010430      *Scrivo sub-file
067400010430     C                   WRITE     S01
067500010430      *
067600010430     C                   ENDIF
067700010430      *
067800010430     C                   ENDSR
067900010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
068000010430
068100010430      *- Imposto campi del record di subfile -*
068200010430     C     SetS01        BEGSR
068300010430      *-----------------------
068400010510     C                   EVAL      S01TbeKe1 = TbeKe1
068500010510      *
068600010510     C                   EVAL      DCMU = TbeUni
068700010510      *
068800010510     C                   EVAL      S01TbeUni = §CMUDesc
068900010510     C                   EVAL      S01Ubicaz = §CMUUbicaz
069000010430      *Imposto visualizzazione flag di annullamento record
069100010430     C                   IF        TbeAtb = '1'
069200010430     C                   EVAL      S01TbeAtb = 'A'
069300010430     C                   ELSE
069400010430     C                   EVAL      S01TbeAtb = ' '
069500010430     C                   ENDIF
069600010430      *
069700010430     C                   ENDSR
069800010430      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
069900010430
070000010426      *-Operazioni iniziali-*
070100000607     C     *INZSR        BEGSR
070200010426      *-----------------------
070300000607     C     *ENTRY        PLIST
070400010503     C                   PARM                    KpjBA
070500000607      *
070600010423     C     K01Tbe01      KLIST
070700010423     C                   KFLD                    TbeCod
070800010424      *
070900010424     C     K02Tbe01      KLIST
071000010424     C                   KFLD                    TbeCod
071100010424     C                   KFLD                    TbeKe1
071200010503      *
071300000607     C                   ENDSR
071400010503      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
071500010503
071600010503      *- Gestione record duplicato-*
071700010426     C     ErrDupKey     BEGSR
071800010503      *-----------------------
071900010426     C                   IF        %ERROR
072000010426     C                             AND
072100010426     C                             %STATUS = 01021
072200010430     C                   SETON                                        9996
072300010426     C                   ENDIF
072400010426
072500010426     C                   ENDSR
072600010503      *-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
072700010503
072800010430      *- Uscita dal PGM -*
072900010430     C     Uscita        BEGSR
073000010430      *-----------------------
073100010503     C                   IF        TbcEsito = *BLANKS
073200010503     C                   EVAL      TbcEsito = Eseguito
073300010430     C                   ENDIF
073400010503      *Restituisco i parametri di ritorno al pgm chiamante
073500010503     C                   EVAL      KpjBu = TnTbcMuS
073600010503      *
073700010430     C                   EVAL      *INLR = *ON
073800010430     C                   RETURN
073900010430      *
074000010430     C                   ENDSR
074100010426      *--------------------------------------------------------------------*
074200010426      *                  E N D   S U B - R O U T I N E S                   *
074300010426      *--------------------------------------------------------------------*
