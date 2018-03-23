000100160606     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('QC2LE')
000200160826
000300171218     FAZORG01L  IF   E           K DISK
000400160606     FPRTF198   O    f  132        PRINTER USROPN
000500140912     F                                     oflind(*inoa)
000600160603
000700160603       // -?Parametri in input?
000800160603     d KPJBA         e ds
000900160603     d UBNSR3CDS       ds
001000160603     D   Ambiente                     1A
001100160603     D   Email_CC                    40A
001200140911
001300160603     D WFX3C00F      e ds
001400160606
001500160606     D TRTCM1DS      e ds
001600160607
001700171218     D*fnlv55ds      e ds
001800160603
001900160603     d Status         sds
002000160603     d  SDSpgm           *proc
002100160603     d  SDSprm           *parms
002200160603     d  SDSuser              254    263
002300160603
002400160603       // -?Stringa SQL da eseguire?
002500160603     d wSql            s           2048    inz  varying
002600160603
002700160603     D fls_save        s                   like(W3CFLS)
002800160606     d Cmd             s            350    inz  varying
002900160606     d CmdEx           s             10I 0
003000160607
003100160607     D* campi per stampa
003200160607     D wNRS            s              2a
003300160607     D wNC             s              7s 0
003400170210     D wW3CCBA         s                   like(W3CCBA)
003500170210     D wTerminal       s                   like(w3cfls)
003600160603
003700160603     d data            s               d
003800160603     d ora             s               t
003900160603
004000160603     D* Verifico segnacolli in tab. "3C" / sul TITAS
004100160603     D TNVR3CR         pr                  extpgm('TNVR3CR')
004200160603     D  kpjba                              likeds(kpjba)
004300160607
004400160607     D* Recupero terminal
004500171218     D*FNLV55R         pr                  extpgm('FNLV55R')
004600171218     D* fnlv55ds                           likeds(fnlv55ds)
004700160606
004800160606     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
004900140911
005000140912     C     *entry        plist
005100160603     C                   parm                    kpjba
005200140912
005300140911      /free
005400160603
005500160603       //--------------------------------------------------------------
005600160603       // M A I N - L I N E
005700160603       //--------------------------------------------------------------
005800160603
005900160603       // Operazioni iniziali?
006000160603       exsr sr_RoutInz;
006100160603
006200160603       // attività richiesta dal chiamante
006300160603       exsr esegui;
006400160603
006500160603       // Operazioni finali?
006600160603       exsr sr_RoutEnd;
006700160603
006800160603       //--------------------------------------------------------------
006900160603       //
007000160603       //--------------------------------------------------------------
007100160603       BEGSR  *inzsr;
007200160603
007300160603         // Inizializzazioni parametri esecuzione comandi sql
007400160603         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
007500160603
007600160603       ENDSR;
007700160603
007800160603       //--------------------------------------------------------------
007900160603       // Operazioni iniziali.
008000160603       //--------------------------------------------------------------
008100160603       BEGSR  sr_RoutInz;
008200160603
008300160826         // se il pgm è chiamato dalla schedulazione (utente EDPCED),
008400160826         // lo eseguo solo i mesi dispari (adesso è eseguita mensilmente ma nella schedulazione
008500160826         // non c'è il bimensile)
008600160826         if SDSUser = 'EDPCED' and
008700160826            %rem(%subdt(%date():*MONTHS) : 2) > 0;
008800160826           exsr sr_RoutEnd;
008900160826         endif;
009000160826
009100160826         // Valorizzo i parametri di ingresso
009200160826         UBNSR3CDS = kpjbu;
009300160606
009400160606         // Verifico segnacolli in tab. "3C" / sul TITAS
009500160606         kpjbu = *blank;
009600160606         TNVR3CR (kpjba);
009700160603
009800160603         data = %date();
009900160603         ora = %time();
010000140911
010100160603         // la stringa SQL è sempre la stessa
010200160606         wSQL =
010300160603             'select * +
010400160603              from wfx3c00f +
010500160603              where w3cnrs > 0 and +
010600160606              (w3cdtr = 0 or +
010700160606               w3cdtr > 0 and +
010800160606               days(curdate()) - +
010900160603               days(date( +
011000160606                (substr(char(w3cdtr) , 1 , 4) concat ''-'' concat +
011100160606                 substr(char(w3cdtr) , 5 , 2) concat ''-'' concat +
011200160603                 substr(char(w3cdtr) , 7 , 2)) +
011300160603               )) > 90 ) and +
011400160606              (w3cduss = 0 or +
011500160606               w3cduss > 0 and +
011600160603               days(curdate()) - +
011700160603               days(date(+
011800160606                (substr(char(w3cduss) , 1 , 4) concat ''-'' concat +
011900160606                 substr(char(w3cduss) , 5 , 2) concat ''-'' concat +
012000160603                 substr(char(w3cduss) , 7 , 2)) +
012100160603               )) > 120 ) +
012200160606              order by w3cfls asc, (w3cnca - w3cncd) desc, w3cnrs asc, +
012300160606              w3cksc asc';
012400160603
012500160603       ENDSR;
012600160603
012700160603       //--------------------------------------------------------------
012800160603       // Esecuzione
012900160603       //--------------------------------------------------------------
013000160603       BEGSR  esegui;
013100160603
013200160603         clear fls_save;
013300160603
013400160603         exec sql  prepare S1  from :wSql;
013500160603         exec sql  declare C1  cursor for S1;
013600160603         exec sql  open C1;
013700140911
013800160603         DoU  SQLCode = 100 or SQLCode < 0;
013900160603           exec sql  fetch next  from C1  into :WFX3C00F;
014000140911
014100160608           // siccome il testo va in email, mon c'è bisogno di ripetere la testata a cambio pagina
014200160608           //if *inOA = *on;
014300160608             //except testata;
014400160608           //endif;
014500140912
014600160603           select;
014700160603            // errore
014800160603            when SQLCode < 0;
014900160603             // - Chiusura cursore (visto che esco dal pgm)
015000160603             exec sql   close C1;
015100160603             exsr sr_RoutEnd;
015200160603            // nessuna lettura
015300160603            when SQLCode = 100;
015400160603             // esco dal ciclo
015500160603            // lettura
015600160603            when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
015700160603             // se ho avuto rottura per filiale segnacollo
015800160603             if w3Cfls <> fls_save;
015900160606               // se il PRTF è aperto, lo chiudo e ne apro un altro
016000160606               if %open(PRTF198);
016100160606                 close PRTF198;
016200160606               endif;
016300160606               exsr aproPrtf;
016400160606               // stampo la testata
016500160606               except testata;
016600160606               // salvo il nuovo dato di rottura
016700160606               fls_save = w3Cfls;
016800160606             endif;
016900160606             // stampo una riga sullo spool aperto
017000160606             wNRS = %editc(w3cnrs : 'X');
017100170210             wNC  = w3cnca-w3cncd+1;
017200170210             // se il codice scambio dati è FTP l'email lo scambia per un hyperlink
017300170210             // per cui "salomonicamente" riempio il campo con _
017400170210             wW3CCBA = *all'_';
017500170210             %subst(wW3CCBA:1:%len(%trim(W3CCBA)))=%trim(W3CCBA);
017600160606             except dettaglio;
017700160603           endsl;
017800160603         enddo;
017900140911
018000160606         // - Chiusura cursore (visto che esco dal pgm)
018100160606         exec sql   close C1;
018200160603
018300160603       ENDSR;
018400160603
018500160603       //--------------------------------------------------------------
018600160606       // Apro il prtf.
018700160603       //--------------------------------------------------------------
018800160606       BEGSR  aproPrtf;
018900160606
019000160607         // recupero il terminal
019100171218         // questo recupero è dal 12/2017 non corretto, si recupera dall'organigramma
019200171218     C***clear fnlv55ds;
019300171218     C***d55tpt = '6';
019400171218     C***d55drf = %dec(%date() : *iso);
019500171218     C***d55lin = w3cfls;
019600171218     C***fnlv55r (fnlv55ds);
019700171218     C***if d55tfa <> 0;
019800171218     C***  wTerminal = d55tfa;
019900171218     C***endif;
020000160607
020100171218         wTerminal = 0;
020200171218     C     W3CFLS        chain     AZORG
020300171218     C                   if        %found(AZORG01L)
020400171218     C                   movel     ORGF70        wTerminal
020500171218     C                   endif
020600171218
020700160606         // preparo i dati per l'OVRPRTF
020800160608         §CM1MITT = 'ced';
020900160606         select;
021000160606          when Ambiente='T';
021100160606           §CM1DST = Email_CC;
021200160606          when Ambiente='P' and Email_CC = *blanks;
021300160607           §CM1DST = 'poc' + %editc(wTerminal : 'X') + '@brt.it';
021400160606          when Ambiente='P' and Email_CC <> *blanks;
021500160607           §CM1DST = 'poc' + %editc(wTerminal : 'X') + '@brt.it; ' +
021600160606                     %trim(Email_CC);
021700160606         endsl;
021800160608         §CM1TIPS = 'X03';
021900160606         §CM1PO   = '046';
022000160607         §CM1VAR  = '*OBJM*Da SEDE/IT/VAS - Fil ' + %editc(W3CFLS : 'X') +
022100160607                    ': verifica serie Disk B inutilizzate';
022200160606         §CM1STS  = 'R';
022300160606         §CM1IDP  = '*';
022400160606
022500160606         Cmd = 'OVRPRTF FILE(PRTF198) OUTQ(EMAILIN) USRDTA(Serie_' +
022600160606                %editc(W3CFLS : 'X') +
022700160606               ') USRDFNDTA(''' +
022800160606               TRTCM1DS + ''')';
022900160606         CMdEx = ExecuteCommand(%trim(Cmd));
023000160606         OPEN PRTF198;
023100160606
023200160606       ENDSR;
023300160606
023400160606       //--------------------------------------------------------------
023500160606       // Operazioni finali.
023600160606       //--------------------------------------------------------------
023700160606       BEGSR  sr_RoutEnd;
023800160606
023900160606         // se il PRTF è aperto, lo chiudo
024000160606         if %open(PRTF198);
024100160606           close PRTF198;
024200160606         endif;
024300160603
024400160603         // Chiusura pgm
024500160603         *inlr = *on;
024600160603         return;
024700160603
024800160603       ENDSR;
024900140912
025000140911      /END-free
025100140911
025200160606     OPRTF198   E            testata           2
025300160608     O                       SDSPGM
025400160606     O                                           71 '*  Verifica serie Disk B i'
025500160606     O                                           84 'nutilizzate *'
025600160608     O          E            testata     2
025700160608     O                                            7 'Cliente'
025800160606     O                                           12 'Ser'
025900160606     O                                           29 'Ragione Sociale'
026000160606     O                                           64 'Segnacolli'
026100160606     O                                           76 'Unific.'
026200160606     O                                           83 'Cod.'
026300160606     O                                           93 'Scambio'
026400160606     O                                          107 'Ultima sped.'
026500160606     O                                          119 'Stampante'
026600160606     O          E            testata     1
026700160606     O                                           64 'assegnati'
026800160606     O                                           84 'Blocco'
026900160606     O                                           92 'Dati'
027000160606     O                                          106 'con serie'
027100160606     O                                          120 'in comodato'
027200140912     O          E            dettaglio   1
027300160606     O                       w3cksc               7
027400160606     O                       wNRS                12
027500160606     O                       w3crsc              49
027600160606     O                       wNC                 64 ' .   .   '
027700160606     O                       w3cksu              76
027800160606     O                       w3cblc              82
027900170210     O                       ww3ccba             92
028000160606     O                       w3cduss            106 '    /  /  '
028100160606     O                       w3cf3r             116
