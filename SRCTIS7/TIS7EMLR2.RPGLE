000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120
000500010918     Ftiabl04l  UF   E           K DISK
000600040107     F***PRTF198   O    F  198        PRINTER
000700010912
000800010912
000900010912     D*------------
001000010912     D* SHIERE COMANDI
001100010912     D*------------
001200050127     D TXT             S            100    DIM(38) CTDATA PERRCD(1)
001300010919     D CMD             S             80    DIM(07) CTDATA PERRCD(1)
001400010912     D*------------
001500010912     D* ARCHITETTURA
001600010912     D*------------
001700010912     D KPJBA         E DS
001800010912     D*------------
001900010919     D* VARIABILI DI WRK
002000010919     D*------------
002100010919     D wrkCMD          S            200
002200060807     D contaMOD        S              5  0 inz(*zeros)
002300060807     D wrkMsgCED       S           5000    inz(*blanks) varying
002400010919     D*------------
002500010912
002600010912
002700000414     C*------------------------------------------------------------------------*
002800000414     C* MAIN LINE
002900000414     C*------------------------------------------------------------------------*
003000000414     C*
003100010919     C* Innanzitutto verifico il profilo utente che sta effettuando l'operazione
003200120305     C* perchè il mailing deve essere spedito come ced@brt.it
003300010919     C* che è attribuito all utente EDPCED
003400010919     C                   IF        KNMUS = 'EDPCED'
003500010919     C                   EXSR      SNDEML
003600010919     C                   ELSE
003700010919     C* Altrimenti mando un messaggio alla coda dell'operatore
003800010919     C                   EVAL      wrkCMD = %trim(CMD(5))+' '+%trim(CMD(6))+
003900010919     C                                      %trim(KNMUS)+')'
004000010919     C                   Z-ADD     200           lengh            15 5
004100010919     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
004200010919     C                   PARM                    wrkCMD
004300010919     C                   PARM                    lengh
004400010919     C                   ENDIF
004500010912     C*
004600010912     C                   SETON                                        LR
004700010912     C*------------------------------------------------------------------------*
004800010912     C* SNDEML - COMPONE TESTO CORPO MESSAGGI OED INVIA E-MAIL
004900010912     C*------------------------------------------------------------------------*
005000010912     C     SNDEML        BEGSR
005100060807     C*
005200060807     C                   movel     *blanks       wrkMsgCED
005300010912     C*
005400010918     C*  Mi posiziono all'inizio del file tiabl04l
005500010918     C     *loval        SETLL     tiabl04l
005600010912     C*  E scorro tutto il file
005700010918     C                   READ      tiabl04l
005800010918     C                   DOW       not %eof(tiabl04l)
005900010912     C*
006000010918     C* Se record è da inviare
006100010918     C                   IF        ablINV = '0'
006200010912     C*
006300010912     C* Inizializzo variabili
006400010912     C                   movel     *blanks       wrkEml          253
006500010912     C                   movel     *blanks       wrkMsg         5000
006600010912     C                   movel     *blanks       wrkOgg           44
006700010912     C* Valorizzo i campi della e-m@ail
006800010919     C                   eval      wrkEml = ablEML
006900120305     C***                eval      wrkEml = 'fabrizio.gurrieri@brt.it'
007000050126     C                   eval      wrkOgg = 'Attivazione ai servizi ON LINE: '+
007100040429     C                                                                 ablUTE
007200010918     C                   eval      wrkMsg = 'Alla c.a.  : '+ablRSP       +':/N'+
007300010912     C                                                                    ':/N'+
007400010913     C                                    %trim(TXT(01))+' '+%trim(TXT(02))+' '+
007500010913     C                                    %trim(TXT(03))+' '+%trim(TXT(04))+' '+
007600010913     C                                    %trim(TXT(05))+' '+%trim(TXT(06))+' '+
007700010913     C                                    %trim(TXT(07))+' '+%trim(TXT(08))+' '+
007800010913     C                                    %trim(TXT(09))+' '+%trim(TXT(10))+' '+
007900010919     C                                    %trim(TXT(11))+' '+%trim(TXT(12))+' '+
008000050126     C                                    ablUTE+%trim(TXT(13))+' '+ablPWD +' '+
008100050126     C                                    %trim(TXT(14))+' '+%trim(TXT(15))+' '+
008200010919     C                                                       %trim(TXT(16))+' '+
008300010913     C                                    %trim(TXT(17))+' '+%trim(TXT(18))+' '+
008400010913     C                                    %trim(TXT(19))+' '+%trim(TXT(20))+' '+
008500010913     C                                    %trim(TXT(21))+' '+%trim(TXT(22))+' '+
008600010913     C                                    %trim(TXT(23))+' '+%trim(TXT(24))+' '+
008700010913     C                                    %trim(TXT(25))+' '+%trim(TXT(26))+' '+
008800010918     C                                    %trim(TXT(27))+' '+%trim(TXT(28))+' '+
008900010918     C                                    %trim(TXT(29))+' '+%trim(TXT(30))+' '+
009000010918     C                                    %trim(TXT(31))+' '+%trim(TXT(32))+' '+
009100010918     C                                    %trim(TXT(33))+' '+%trim(TXT(34))+' '+
009200010918     C                                    %trim(TXT(35))+' '+%trim(TXT(36))+' '+
009300050224     C                                    %trim(TXT(37))+' '+%trim(TXT(38))+' '+
009400050224     C                                    ':/N'
009500060807     C*
009600060807     C* Compogo il messaggio da inviare a ced con il resoconto d quanto spedito
009700060809     C                   EVAL      wrkMsgCED = %trim(wrkMsgCED) + 'Cliente: '
009800060809     C                             + ablUTE + '   E-mail: ' + ablEML + ':/N'
009900010912     C*
010000010912     C                   CALL(e)   'TIS701C'
010100010912     C                   PARM                    wrkEml
010200010912     C                   PARM                    wrkOgg
010300010912     C                   PARM                    wrkMsg
010400010919     C*
010500010919     C                   EVAL      contaMOD = contaMOD + 1
010600010914     C*
010700010914     C* Dopo l'invio fleggo il record come "già inviato"
010800010919     C                   EVAL      ablINV = '1'
010900010919     C                   UPDATE    tiabl000
011000010913     C*
011100010918     C* DOPO OGNI E-MAIL INVIATA ATTENDO "x" SECONDI
011200010919     C*                  Z-ADD     80            lengh            15 5
011300010919     C*                  CALL      'QCMDEXC'                                    *LANCIA CMD
011400010919     C*                  PARM                    CMD(1)
011500010919     C*                  PARM                    lengh
011600010912     C*
011700010912     C                   ENDIF
011800010912     C* Proseguo con la lettura
011900010918     C                   READ      tiabl04l
012000010912     C*
012100010912     C                   ENDDO
012200060807     C*
012300060807     C* Infine mando email a ced con resoconto invii effettuati
012400120305     C                   eval      wrkEml = 'ced@brt.it'
012500060807     C                   eval      wrkOgg = 'Attivazione ai servizi ON LINE'
012600060809     C                   eval      wrkMsgCED = ':/N' + 'Inviate '              +
012700060809     C                                         %editc(contaMOD:'4')            +
012800060807     C                                         ' e-mail, segue dettaglio: '    +
012900060809     C                                         ':/N' + %trim(wrkMsgCED)
013000060807     C                   CALL(e)   'TIS701C'
013100060807     C                   PARM                    wrkEml
013200060807     C                   PARM                    wrkOgg
013300060807     C                   PARM                    wrkMsgCED
013400010919     C*
013500040107     C***                EXCEPT    listatot
013600010912     C*
013700010912     C                   ENDSR
013800010912     C*------------------------------------------------------------------------*
013900010912     C* *INZSR - ROUTINE DI INIZIALIZZAZIONE *PGM
014000010912     C*------------------------------------------------------------------------*
014100010912     C     *INZSR        BEGSR
014200010912     C*---
014300010912     C* RICEVIMENTO PARAMETRI
014400010912     C*---
014500010912     C     *ENTRY        PLIST
014600010912     C                   PARM                    KPJBA
014700010912     C*
014800010912     C                   ENDSR
014900010919
015000040107     O***PRTF198   E            listatot    1
015100040107     O***                       contaMOD            +1
015200010913** TXT - TESTO PER CORPO E-MAIL
015300120305:/NABILITAZIONE ALL'UTILIZZO DEI SERVIZI ON LINE BRT
015400010918:/N
015500010918:/NGentile Cliente,
015600050126:/N
015700050126:/Ndesideriamo comunicarle che come da sua richiesta abbiamo provveduto ad
015800050126abilitarla all'utilizzo dei nostri servizi ON LINE.
015900010919:/N
016000120305:/NL'indirizzo del nostro sito è http://www.brt.it
016100050126:/NDalla home page, inserendo negli appositi spazi il codice utente e la password
016200050126qui di seguito forniti, potrà accedere ai servizi a Lei riservati.
016300010919:/N
016400010919:/NUTENTE  :
016500010919:/NPASSWORD:
016600010918:/N
016700050126:/NLa password indicata potrà essere modificata, dopo aver effettuato il Login,
016800050126tramite la funzione "Cambio password".
016900050126:/N
017000050126:/NLa password è un dato personale che deve essere trattato con la massima cura.
017100050126I destinatari delle password si impegnano a sensibilizzare e ad istruire il proprio
017200050126personale delegato all'utilizzo, assumendosi tutte le responsabilità su eventuale
017300050126uso non conforme alle normali norme di riservatezza. La password non deve essere ceduta
017400050126a terzi e deve essere data in uso esclusivamente al personale che abbia necessaria
017500050126delega per accedere ai dati riservati. Il tempo di collegamento e del relativo uso
017600050126delle password deve essere limitato al tempo strettamente necessario al reperimento
017700050126ed alla consultazione delle informazioni disponibili o all'immissione dei dati richiesti.
017800050126:/NQualsiasi utilizzo diverso dalla semplice consultazione dei dati relativi alle
017900050126spedizioni potrebbe portare a perdite o modifiche ai dati stessi delle quali non saremo
018000050126responsabili e che, in casi estremi, potrebbero integrare i reati di cui agli articoli
018100050126615 ter, quater, quinquies e 617 quater, quinquies e sexies del codice penale.
018200010918:/N
018300050126:/NPer dubbi, problemi tecnici o suggerimenti, non esiti a contattarci all'
018400120305indirizzo: ced@brt.it
018500050126:/N
018600050126:/NLe ricordiamo inoltre che per l'assistenza commerciale dovrà rivolgersi
018700050126al Punto Operativo di zona.
018800010918:/N
018900010919:/NCon l'occasione porgiamo cordiali saluti.
019000110524:/NBRT S.p.A.
019100010913** CMD - COMANDI CL
019200010918DLYJOB DLY(1)
019300010919SNDBRKMSG MSG('ATTENZIONE, il mailing del attivazione ai servizi VAS
019400010919Internet deve essere effettuata dall utente EDPCED!')
019500010919TOMSGQ(DSP01) MSGTYPE(*INQ)
019600010919SNDMSG MSG('ATTENZIONE, il mailing del attivazione ai servizi VAS
019700010919Internet deve essere effettuata dall utente EDPCED!') TOUSR(
