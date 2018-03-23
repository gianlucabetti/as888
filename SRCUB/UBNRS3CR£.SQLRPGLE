000100160606     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('QC2LE')
000200160826
000300160606     FPRTF198   O    f  132        PRINTER USROPN
000400140912     F                                     oflind(*inoa)
000500160603
000600160603       // -?Parametri in input?
000700160603     d KPJBA         e ds
000800160603     d UBNSR3CDS       ds
000900160603     D   Ambiente                     1A
001000160603     D   Email_CC                    40A
001100140911
001200160603     D WFX3C00F      e ds
001300160606
001400160606     D TRTCM1DS      e ds
001500160607
001600160607     D fnlv55ds      e ds
001700160603
001800160603     d Status         sds
001900160603     d  SDSpgm           *proc
002000160603     d  SDSprm           *parms
002100160603     d  SDSuser              254    263
002200160603
002300160603       // -?Stringa SQL da eseguire?
002400160603     d wSql            s           2048    inz  varying
002500160603
002600160603     D fls_save        s                   like(W3CFLS)
002700160606     d Cmd             s            350    inz  varying
002800160606     d CmdEx           s             10I 0
002900160607
003000160607     D* campi per stampa
003100160607     D wNRS            s              2a
003200160607     D wNC             s              7s 0
003300170210     D wW3CCBA         s                   like(W3CCBA)
003400170210     D wTerminal       s                   like(w3cfls)
003500160603
003600160603     d data            s               d
003700160603     d ora             s               t
003800160603
003900160603     D* Verifico segnacolli in tab. "3C" / sul TITAS
004000160603     D TNVR3CR         pr                  extpgm('TNVR3CR')
004100160603     D  kpjba                              likeds(kpjba)
004200160607
004300160607     D* Recupero terminal
004400160607     D FNLV55R         pr                  extpgm('FNLV55R')
004500160607     D  fnlv55ds                           likeds(fnlv55ds)
004600160606
004700160606     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
004800140911
004900140912     C     *entry        plist
005000160603     C                   parm                    kpjba
005100140912
005200140911      /free
005300160603
005400160603       //--------------------------------------------------------------
005500160603       // M A I N - L I N E
005600160603       //--------------------------------------------------------------
005700160603
005800160603       // Operazioni iniziali?
005900160603       exsr sr_RoutInz;
006000160603
006100160603       // attività richiesta dal chiamante
006200160603       exsr esegui;
006300160603
006400160603       // Operazioni finali?
006500160603       exsr sr_RoutEnd;
006600160603
006700160603       //--------------------------------------------------------------
006800160603       //
006900160603       //--------------------------------------------------------------
007000160603       BEGSR  *inzsr;
007100160603
007200160603         // Inizializzazioni parametri esecuzione comandi sql
007300160603         exec sql  set option  dynusrprf = *owner, closqlcsr = *endmod;
007400160603
007500160603       ENDSR;
007600160603
007700160603       //--------------------------------------------------------------
007800160603       // Operazioni iniziali.
007900160603       //--------------------------------------------------------------
008000160603       BEGSR  sr_RoutInz;
008100160603
008200160826         // se il pgm è chiamato dalla schedulazione (utente EDPCED),
008300160826         // lo eseguo solo i mesi dispari (adesso è eseguita mensilmente ma nella schedulazione
008400160826         // non c'è il bimensile)
008500160826         if SDSUser = 'EDPCED' and
008600160826            %rem(%subdt(%date():*MONTHS) : 2) > 0;
008700160826           exsr sr_RoutEnd;
008800160826         endif;
008900160826
009000160826         // Valorizzo i parametri di ingresso
009100160826         UBNSR3CDS = kpjbu;
009200160606
009300160606         // Verifico segnacolli in tab. "3C" / sul TITAS
009400160606         kpjbu = *blank;
009500160606         TNVR3CR (kpjba);
009600160603
009700160603         data = %date();
009800160603         ora = %time();
009900140911
010000160603         // la stringa SQL è sempre la stessa
010100160606         wSQL =
010200160603             'select * +
010300160603              from wfx3c00f +
010400160603              where w3cnrs > 0 and +
010500160606              (w3cdtr = 0 or +
010600160606               w3cdtr > 0 and +
010700160606               days(curdate()) - +
010800160603               days(date( +
010900160606                (substr(char(w3cdtr) , 1 , 4) concat ''-'' concat +
011000160606                 substr(char(w3cdtr) , 5 , 2) concat ''-'' concat +
011100160603                 substr(char(w3cdtr) , 7 , 2)) +
011200160603               )) > 90 ) and +
011300160606              (w3cduss = 0 or +
011400160606               w3cduss > 0 and +
011500160603               days(curdate()) - +
011600160603               days(date(+
011700160606                (substr(char(w3cduss) , 1 , 4) concat ''-'' concat +
011800160606                 substr(char(w3cduss) , 5 , 2) concat ''-'' concat +
011900160603                 substr(char(w3cduss) , 7 , 2)) +
012000160603               )) > 120 ) +
012100160606              order by w3cfls asc, (w3cnca - w3cncd) desc, w3cnrs asc, +
012200160606              w3cksc asc';
012300160603
012400160603       ENDSR;
012500160603
012600160603       //--------------------------------------------------------------
012700160603       // Esecuzione
012800160603       //--------------------------------------------------------------
012900160603       BEGSR  esegui;
013000160603
013100160603         clear fls_save;
013200160603
013300160603         exec sql  prepare S1  from :wSql;
013400160603         exec sql  declare C1  cursor for S1;
013500160603         exec sql  open C1;
013600140911
013700160603         DoU  SQLCode = 100 or SQLCode < 0;
013800160603           exec sql  fetch next  from C1  into :WFX3C00F;
013900140911
014000160608           // siccome il testo va in email, mon c'è bisogno di ripetere la testata a cambio pagina
014100160608           //if *inOA = *on;
014200160608             //except testata;
014300160608           //endif;
014400140912
014500160603           select;
014600160603            // errore
014700160603            when SQLCode < 0;
014800160603             // - Chiusura cursore (visto che esco dal pgm)
014900160603             exec sql   close C1;
015000160603             exsr sr_RoutEnd;
015100160603            // nessuna lettura
015200160603            when SQLCode = 100;
015300160603             // esco dal ciclo
015400160603            // lettura
015500160603            when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
015600160603             // se ho avuto rottura per filiale segnacollo
015700160603             if w3Cfls <> fls_save;
015800160606               // se il PRTF è aperto, lo chiudo e ne apro un altro
015900160606               if %open(PRTF198);
016000160606                 close PRTF198;
016100160606               endif;
016200160606               exsr aproPrtf;
016300160606               // stampo la testata
016400160606               except testata;
016500160606               // salvo il nuovo dato di rottura
016600160606               fls_save = w3Cfls;
016700160606             endif;
016800160606             // stampo una riga sullo spool aperto
016900160606             wNRS = %editc(w3cnrs : 'X');
017000170210             wNC  = w3cnca-w3cncd+1;
017100170210             // se il codice scambio dati è FTP l'email lo scambia per un hyperlink
017200170210             // per cui "salomonicamente" riempio il campo con _
017300170210             wW3CCBA = *all'_';
017400170210             %subst(wW3CCBA:1:%len(%trim(W3CCBA)))=%trim(W3CCBA);
017500160606             except dettaglio;
017600160603           endsl;
017700160603         enddo;
017800140911
017900160606         // - Chiusura cursore (visto che esco dal pgm)
018000160606         exec sql   close C1;
018100160603
018200160603       ENDSR;
018300160603
018400160603       //--------------------------------------------------------------
018500160606       // Apro il prtf.
018600160603       //--------------------------------------------------------------
018700160606       BEGSR  aproPrtf;
018800160606
018900160607         // recupero il terminal
019000160607         clear fnlv55ds;
019100160607         d55tpt = '6';
019200160607         d55drf = %dec(%date() : *iso);
019300160607         d55lin = w3cfls;
019400160607         fnlv55r (fnlv55ds);
019500160607         if d55tfa <> 0;
019600160607           wTerminal = d55tfa;
019700160607         endif;
019800160607
019900160606         // preparo i dati per l'OVRPRTF
020000160608         §CM1MITT = 'ced';
020100160606         select;
020200160606          when Ambiente='T';
020300160606           §CM1DST = Email_CC;
020400160606          when Ambiente='P' and Email_CC = *blanks;
020500160607           §CM1DST = 'poc' + %editc(wTerminal : 'X') + '@brt.it';
020600160606          when Ambiente='P' and Email_CC <> *blanks;
020700160607           §CM1DST = 'poc' + %editc(wTerminal : 'X') + '@brt.it; ' +
020800160606                     %trim(Email_CC);
020900160606         endsl;
021000160608         §CM1TIPS = 'X03';
021100160606         §CM1PO   = '046';
021200160607         §CM1VAR  = '*OBJM*Da SEDE/IT/VAS - Fil ' + %editc(W3CFLS : 'X') +
021300160607                    ': verifica serie Disk B inutilizzate';
021400160606         §CM1STS  = 'R';
021500160606         §CM1IDP  = '*';
021600160606
021700160606         Cmd = 'OVRPRTF FILE(PRTF198) OUTQ(EMAILIN) USRDTA(Serie_' +
021800160606                %editc(W3CFLS : 'X') +
021900160606               ') USRDFNDTA(''' +
022000160606               TRTCM1DS + ''')';
022100160606         CMdEx = ExecuteCommand(%trim(Cmd));
022200160606         OPEN PRTF198;
022300160606
022400160606       ENDSR;
022500160606
022600160606       //--------------------------------------------------------------
022700160606       // Operazioni finali.
022800160606       //--------------------------------------------------------------
022900160606       BEGSR  sr_RoutEnd;
023000160606
023100160606         // se il PRTF è aperto, lo chiudo
023200160606         if %open(PRTF198);
023300160606           close PRTF198;
023400160606         endif;
023500160603
023600160603         // Chiusura pgm
023700160603         *inlr = *on;
023800160603         return;
023900160603
024000160603       ENDSR;
024100140912
024200140911      /END-free
024300140911
024400160606     OPRTF198   E            testata           2
024500160608     O                       SDSPGM
024600160606     O                                           71 '*  Verifica serie Disk B i'
024700160606     O                                           84 'nutilizzate *'
024800160608     O          E            testata     2
024900160608     O                                            7 'Cliente'
025000160606     O                                           12 'Ser'
025100160606     O                                           29 'Ragione Sociale'
025200160606     O                                           64 'Segnacolli'
025300160606     O                                           76 'Unific.'
025400160606     O                                           83 'Cod.'
025500160606     O                                           93 'Scambio'
025600160606     O                                          107 'Ultima sped.'
025700160606     O                                          119 'Stampante'
025800160606     O          E            testata     1
025900160606     O                                           64 'assegnati'
026000160606     O                                           84 'Blocco'
026100160606     O                                           92 'Dati'
026200160606     O                                          106 'con serie'
026300160606     O                                          120 'in comodato'
026400140912     O          E            dettaglio   1
026500160606     O                       w3cksc               7
026600160606     O                       wNRS                12
026700160606     O                       w3crsc              49
026800160606     O                       wNC                 64 ' .   .   '
026900160606     O                       w3cksu              76
027000160606     O                       w3cblc              82
027100170210     O                       ww3ccba             92
027200160606     O                       w3cduss            106 '    /  /  '
027300160606     O                       w3cf3r             116
