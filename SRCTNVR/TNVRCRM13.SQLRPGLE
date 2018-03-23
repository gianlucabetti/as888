000100100413      //---------------------------------------------------------------
000200120702      //?TNVRCRM13 - Aggiorna data primo contatto su tutti i potenziali
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000120911     fCNACO16L  if   e           k disk
001100120912     fCNACVT1L  if   e           k disk
001200120911     fCNCLP00F  if   e           k disk
001300120912     fTIATC05L  if   e           k disk
001400120912     fTICPS11L  if   e           k disk
001500120912     fTNCPO01L  uf   e           k disk
001600100413
001700100413      //---------------------------------------------------------------
001800100413      //?Definizione costanti.
001900100413      //---------------------------------------------------------------
002000100413
002100100413      //---------------------------------------------------------------
002200100413      //?Definizione schiere.
002300100413      //---------------------------------------------------------------
002400090715
002500100413      //---------------------------------------------------------------
002600100413      //?Definizione aree dati.
002700100413      //---------------------------------------------------------------
002800100413
002900100413      //---------------------------------------------------------------
003000100413      //?Definizione strutture dati.
003100100413      //---------------------------------------------------------------
003200120702
003300120702      // Campo CPORST
003400120702     d dCPO01        e ds
003500120911
003600120911      // Controllo ed inversione date?
003700120911     d WLBdat          ds                  inz
003800120911     d  G08dat                        8  0 inz
003900120911     d  G08inv                        8  0 inz
004000120911     d  G08err                        1    inz('3')
004100120911     d  G08tgi                        5  0 inz
004200100413
004300100413      //---------------------------------------------------------------
004400100413      //?Definizione variabili globali.
004500100413      //---------------------------------------------------------------
004600100413
004700100413      // - Campi di comodo
004800120702     d wCPO            s                   like(CPOcpo) inz
004900120702     d wData           s              8  0
005000120912     d wData6          s              6  0
005100100413
005200100413      //---------------------------------------------------------------
005300100413      //?Definizione procedure esterne.
005400100413      //---------------------------------------------------------------
005500120911
005600120911      /copy gaitrasrc/srcProtoPr,XSRDA8
005700100413
005800100413      //---------------------------------------------------------------
005900120911      //?Prototipi.
006000100413      //---------------------------------------------------------------
006100100413
006200100413      //---------------------------------------------------------------
006300100413      //?Definizione key-list.
006400100413      //---------------------------------------------------------------
006500100413
006600100413      //---------------------------------------------------------------
006700100413      //?Riepilogo indicatori.
006800100413      //---------------------------------------------------------------
006900100413
007000100413      //---------------------------------------------------------------
007100100413      //?M A I N - L I N E
007200100413      //---------------------------------------------------------------
007300100413
007400100413      /free
007500100413
007600100413       //?Operazioni iniziali
007700100513       exsr RoutInz;
007800100413
007900120702       //?Elaborazione principale
008000100510       exsr Elabora;
008100100413
008200100413       //?Operazioni finali
008300100513       exsr RoutEnd;
008400100413
008500100413       //--------------------------------------------------------------
008600100413       //?Operazioni iniziali.
008700100413       //--------------------------------------------------------------
008800100513       BEGSR RoutInz;
008900100413
009000100510         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009100100413
009200100413       ENDSR;
009300100413
009400100413       //--------------------------------------------------------------
009500100510       //?Elabora.
009600100413       //--------------------------------------------------------------
009700100510       BEGSR Elabora;
009800100511
009900120702       //?Leggo tutti i potenziali
010000120702         setll (wCPO) TNCPO01L;
010100120702         DOW not %eof(TNCPO01L);
010200120702           read TNCPO01L;
010300120702           IF  %eof(TNCPO01L);
010400120702             leavesr;
010500120702           ENDIF;
010600120702
010700120702           dCPO01 = CPOrst;
010800100510
010900120702         //?Recupero la data più vecchia dagli Stati del potenziale
011000120702           exec sql
011100120702           select min(CPSdst) into :wData
011200120702           from TICPS00F
011300120702           where CPScpo = :CPOCPO;
011400120702
011500120912         //?Non trovo nessuno stato faccio altri controlli per recuperare la data
011600120702           IF sqlcod < 0;
011700120912             exsr Altra_Data;
011800120702           ENDIF;
011900120911
012000120911           IF  wData > 0;
012100120702
012200120912           //?Se la data sul Potenziale è a 0 oppure è più alta rispetto a
012300120912           //?quella degli stati/data prima spedizione  la aggiorno
012400120912             IF  §CPOdtpcon <= *zeros or
012500120912                (§CPOdtpcon >  *zeros and §CPOdtpcon > %editc(wData:'X'));
012600120912               §CPOdtpcon = %editc(wData:'X');
012700120912             ENDIF;
012800120911
012900120911           ENDIF;
013000120702
013100120702           CPOrst = dCPO01;
013200120702
013300120702           update TNCPO000;
013400120702
013500120702         ENDDO;
013600100413
013700100413       ENDSR;
013800120911
013900120911       //--------------------------------------------------------------
014000120912       //?Controlli vari per recupero data primo contatto.
014100120911       //--------------------------------------------------------------
014200120912       BEGSR Altra_Data;
014300120911
014400120912         clear wData;
014500120912         clear wData6;
014600120912
014700120912       //?Come primo controllo cerco la data più vecchia
014800120912       //?come data prima spedizione
014900120912       //?leggendo tutti i clienti legati al potenziale
015000120911         setll (CPOcpo) CNACO16L;
015100120911         DOW not %eof(CNACO16L);
015200120911           reade (CPOcpo) CNACO16L;
015300120911           IF  %eof(CNACO16L);
015400120911             leave;
015500120911           ENDIF;
015600120911
015700120912         //?recupero la data prima spedizione
015800120911           chain (ACOkut:ACOkcc:ACOksc) CNCLP00F;
015900120911           IF  not %found(CNCLP00F);
016000120911             iter;
016100120911           ENDIF;
016200120911
016300120912         //?tengo la data più vecchia
016400120912           IF  wData6 = 0 or wData6 > CLPdps;
016500120912             wData6 = CLPdps;
016600120911           ENDIF;
016700120911
016800120911         ENDDO;
016900120912
017000120912         //?Ho trovato una data la imposto lunga 8 e vado via
017100120912         IF  wData6 > 0;
017200120912           reset WLBdat;
017300120912           G08inv = wData6;
017400120912           xsrda8 (WLBdat);
017500120912           IF  G08err = *off;
017600120912             wData = G08inv;
017700120912             leavesr;
017800120912           ENDIF;
017900120912         ENDIF;
018000120911
018100120912       //?Se arrivo qua vuol dire che non ho trovato la data prima spedizione
018200120912       //?quindi provo a cercare la data di inserimento anagrafica
018300120912         setll (CPOcpo) CNACO16L;
018400120912         DOW not %eof(CNACO16L);
018500120912           reade (CPOcpo) CNACO16L;
018600120912           IF  %eof(CNACO16L);
018700120912             leave;
018800120912           ENDIF;
018900120912         //?cerco la data inserimento sulle variazioni
019000120912           setll (ACOkcc:ACOksc) CNACVT1L;
019100120912           DOW  not %eof(CNACVT1L);
019200120912             reade (ACOkcc:ACOksc) CNACVT1L;
019300120912             IF  %eof(CNACVT1L);
019400120912               leave;
019500120912             ENDIF;
019600120912           //?non considero le Modifiche
019700120912             IF  ACVcta = 'M';
019800120912               iter;
019900120912             ENDIF;
020000120912           //?tengo la data più vecchia
020100120912             IF  wData = 0 or wData > ACVdav;
020200120912               wData = ACVdav;
020300120912             ENDIF;
020400120912           ENDDO;
020500120912         ENDDO;
020600120912
020700120912         //?Ho trovato la data vado via
020800120912         IF  wData > 0;
020900120912           leavesr;
021000120912         ENDIF;
021100120912
021200120912       //?Come ultimo tentativo
021300120912       //?verifico se il potenziale è Eliminabile
021400120912         IF  CPOfls <> 'E';
021500120912           leavesr;
021600120912         ENDIF;
021700120912
021800120912       //?in questo caso cerco la data dell'attività, tengo sempre la più vecchia
021900120912         setll (CPOcpo) TIATC05L;
022000120912         DOW not %eof(TIATC05L);
022100120912           reade (CPOcpo) TIATC05L;
022200120912           IF  %eof(TIATC05L);
022300120912             leave;
022400120912           ENDIF;
022500120912         //?tengo la data più vecchia
022600120912           IF  wData = 0 or wData > ATCdco;
022700120912             wData = ATCdco;
022800120912           ENDIF;
022900120912         ENDDO;
023000120911
023100120911       ENDSR;
023200100413
023300100413       //--------------------------------------------------------------
023400100413       //?Operazioni finali.
023500100413       //--------------------------------------------------------------
023600100513       BEGSR RoutEnd;
023700100413
023800100413         *inLR = *on;
023900100413         return;
024000100413
024100100413       ENDSR;
024200100413
024300100413      /end-free
