000100130213      //---------------------------------------------------------------
000200130717      //?TRULQLKR - Crea File Spia per creazione dati su QLIK
000300130213      //---------------------------------------------------------------
000400130213     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500161111     h dftactgrp(*no) actgrp(*caller)
000600130213
000700130213      //---------------------------------------------------------------
000800130213      //?Dichiarazione file.
000900130213      //---------------------------------------------------------------
001000161111     fAZQLK01L  uf a e           k disk    usropn
001100161111
001200161111       // - Esecuzione comando
001300161111      /copy gaitrasrc/srcprotopr,SYSTEM
001400130213
001500130213      //---------------------------------------------------------------
001600130213      //?Definizione strutture dati.
001700130213      //---------------------------------------------------------------
001800130213
001900130213      //?Parametri ricevuti
002000130213     d KPJBA         e ds
002100161111     D TIBS55        E DS                  EXTNAME(TIBS55DS)
002200130213
002300130213      //---------------------------------------------------------------
002400130213      //?Definizione variabili globali.
002500130213      //---------------------------------------------------------------
002600130213
002700130717      //?Campi di comodo
002800130717     d wTPela          s             10a
002900161111     d cmd             s            512a   varying
003000130213
003100130213
003200130213      //---------------------------------------------------------------
003300130213      //?M A I N - L I N E
003400130213      //---------------------------------------------------------------
003500130213
003600130213     c     *Entry        plist
003700130213     c                   parm                    KPJBA
003800161111
003900161111     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
004000161111     C* da/a cui spedire
004100161111     C                   CLEAR                   TIBS55
004200161111     C                   MOVEL     'L'           I50TLA
004300161111     C                   MOVEL     '046'         I50PPO
004400161111     C                   MOVEL     '046'         I50APO
004500161111     C                   CALL      'TIBS55R'
004600161111     C                   PARM                    TIBS55
004700161111      **
004800161111      *** OVR del file
004900161111      **
005000130213      /free
005100161111       //?esegue OVRDBF prima di aprire il file
005200161111                 cmd = 'OVRDBF file(AZQLK01L) tofile(' +
005300161115                        %Trim(O50ALR) + '/AZQLK01L)';
005400161111
005500161111               ExecuteCommand (cmd);
005600161111               open AZQLK01L;
005700130213
005800130717       wTPela = %subst(kpjbu:1:10);
005900130717
006000130717       IF  wTPela <> *blanks;
006100130717
006200130717       //?Aggancio il file con il tipo elaborazione passato
006300130717         chain wTPela AZQLK01L;
006400130717           QLKtck = %char(%timestamp:*iso0);
006500130726           clear QLKqlk ;
006600130717         IF  %found(AZQLK01L);
006700130717           UPDATE  AZQLK000;
006800130717         ELSE;
006900130717           QLKela = wTPela;
007000130717           WRITE  AZQLK000;
007100130717         ENDIF;
007200130717
007300130717       ENDIF;
007400130213
007500161111         *inLR = *on;
007600130213
007700130213      /end-free
007800161111     c                   close     AZQLK01L
007900161111     c                   RETURN
