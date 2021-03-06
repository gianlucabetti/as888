000100110225     ***************************************************************************
000200110225     **
000300110301     ** Tester di TRULSNDMM.
000400110225     **
000500110225     ***************************************************************************
000600110303     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000700110228
000800110228     ***************************************************************************
000900110228     **
001000110228     ** Costanti.
001100110228     **
001200110228     ***************************************************************************
001300110309      /COPY GAITRASRC/SRCCONST,TRULSNDMM
001400110303     D CRLF...
001500110303     D                 C                   X'0D25'
001600110303
001700110225     ***************************************************************************
001800110225     **
001900110225     ** Strutture dati.
002000110225     **
002100110225     ***************************************************************************
002200110225
002300110225     ***************************************************************************
002400110225     **
002500110225     ** Campi.
002600110225     **
002700110225     ***************************************************************************
002800110303     D body            S          32767A   VARYING
002900110309     D style           S          32767A   VARYING
003000110303     D inviare         S               N
003001110310     D profiloUtente   S             10A
003100110302
003200110225     ***************************************************************************
003300110225     **
003400110225     ** Prototipi.
003500110225     **
003600110225     ***************************************************************************
003700110309      /COPY GAITRASRC/SRCPROTOPR,TRULSNDMM
003800110303
003900110303     ***************************************************************************
004000110303     **
004100110303     ** Parametri.
004200110303     **
004300110303     ***************************************************************************
004400110303     C     *ENTRY        PLIST
004500110303     C                   PARM                    inviare
004501110310     C                   PARM                    profiloUtente
004600110303
004700110301     ***************************************************************************
004800110301     **
004900110301     ** Main.
005000110301     **
005100110301     ***************************************************************************
005200110228
005300110301      /FREE
005400110301
005500110301       *INLR = *ON;
005600110301
005700110301       IF SNDMM_Init() <> SNDMM_ESITO_OK;
005800110301         RETURN;
005900110301       ENDIF;
006000110301
006100110301       IF SNDMM_NewMail( *OMIT
006200110309                       : 'Formati delle date.'
006300110301                       : 'text/html'
006400110309                       : *OMIT
006500110309                       : 'CUS'
006600110301                       ) <> SNDMM_ESITO_OK;
006700110301         SNDMM_Finalize();
006800110301         RETURN;
006900110301       ENDIF;
007000110301
007100110310       SNDMM_AddRecipientByUserProfile( profiloUtente
007200110309                                      : SNDMM_DISTRIBUTION_TYPE_NORMAL
007300110309                                      );
007400110309
007500110310       SNDMM_AddRecipientByUserProfile( 'EDPDC'
007600110309                                      : SNDMM_DISTRIBUTION_TYPE_CARBON_COPY
007700110309                                      );
007701110311
007702110311       SNDMM_AddRecipient( 'danilo.cussini' + SNDMM_SM050000_CP1144
007703150401                         + 'brt.it'
007704110311                         : SNDMM_DISTRIBUTION_TYPE_CARBON_COPY
007705110311                         : 'Danilo Cussini'
007706110311                         );
008200110309
008300150401       SNDMM_AddRecipient( 'danilo.cussini' + SNDMM_SM050000_CP1144
008400150401                         + 'live.com'
008500110309                         : SNDMM_DISTRIBUTION_TYPE_BLIND_CARBON_COPY
008600110309                         : 'D. Cussini'
008700110309                         );
008800110309
009000110310       body = '<h3>Formati delle date</h3>' + CRLF
009001110309            + '<p>' + CRLF
009100110309            + 'I formati standard delle date sono:' + CRLF
009200110309            + '</p>' + CRLF
009300110309            + '<table>' + CRLF
009400150401            + '<thead>' + CRLF
009500110310            + '<tr>'
009600110310            + '<th>Formato</th>'
009700110310            + '<th>Sigla</th>'
009800110310            + '<th>Esempio</th>'
009900110309            + '</tr>' + CRLF
010000150401            + '</thead>' + CRLF
010100110311            + '<tbody style="font-family: Courier New;">' + CRLF
010200110310            + '<tr>'
010300110310            + '<td>yyyy-mm-dd</td>'
010400110310            + '<td>*ISO</td>'
010500110310            + '<td>2010-12-31</td>'
010600110310            + '</tr>' + CRLF
010700110310            + '<tr class="riga_pari">'
010800110310            + '<td>dd.mm.yyyy</td>'
010900110310            + '<td>*EUR</td>'
011000110310            + '<td>31.12.2010</td>'
011100110309            + '</tr>' + CRLF
011200110310            + '<tr>'
011300110310            + '<td>mm/dd/yyyy</td>'
011400110310            + '<td>*USA</td>'
011500110310            + '<td>12/31/2010</td>'
011600110309            + '</tr>' + CRLF
011700110309            + '</tbody>' + CRLF
011800110309            + '</table>' + CRLF
011900110310            + '<p><strong>' + CRLF
012000110309            + 'Il formato dd/mm/yyyy non � un formato standard, quindi non +
012100110309              deve essere usato perch� si confonde col formato americano.'
012101110309            + CRLF
012102110310            + '</strong></p>' + CRLF
012103110309            + '<p>' + CRLF
012200110309            + 'Per esempio, con 11/09/2001 non si indica la famosa data del +
012300110309              11 settembre, ma il 9 novembre.' + CRLF
012400110309            + '</p>' + CRLF
012600110309            ;
012700110309
012800110310       SNDMM_AddBody(body);
012900110309       SNDMM_EndMail(inviare);
013000110301       SNDMM_Finalize();
013100110301
013200110301      /END-FREE
