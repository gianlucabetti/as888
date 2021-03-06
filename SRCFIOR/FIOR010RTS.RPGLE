000100110412      //--------------------------------------------------------------
000200150528      //?FIOR010RTSR - TEST DEMO FIOR010R
000300110412      //--------------------------------------------------------------
000400110412
000500110412     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600110412     h dftactgrp(*no) actgrp(*caller)
000700110412
000800110412      //---------------------------------------------------------------
000900110412      //?Dichiarazione file.
001000110412      //---------------------------------------------------------------
001100110412
001200110412      // - Video
001300150528     fFIOR010DTScf   e             workstn
001400110412
001500110412      //---------------------------------------------------------------
001600110412      //?Definizione aree dati.
001700110412      //---------------------------------------------------------------
001800110412
001900110412      //---------------------------------------------------------------
002000110412      //?Definizione strutture dati.
002100110412      //---------------------------------------------------------------
002200110412
002300110412      // - Parametri ricevuti
002400110412     d KPJBA         e ds
002500110412
002600150528     d FIOR010DS     e ds                  inz
002700150528     d FIDNA1DS      e ds                  inz
002800110412
002900110412      //---------------------------------------------------------------
003000110412      //?Definizione variabili globali.
003100110412      //---------------------------------------------------------------
003200110412
003300110412      // - Flags booleani
003400110412     d $Fine           s               n   inz(*off)
003500110412
003600110412      //---------------------------------------------------------------
003700110412      //?Definizione procedure usate.
003800110412      //---------------------------------------------------------------
003900110412
004000110412      // - Interrogazione ORM
004100150528     d fior010r        pr                  extpgm('FIOR010R')
004200110412     d  kpjba                              likeds(KPJBA)
004300150528     d  fior010ds                          likeds(FIOR010DS)
004400150528     d                                     options(*NOPASS)
004500150528     d  fiodna1ds                          likeds(FIDNA1DS)
004600150528     d                                     options(*NOPASS)
004700110412
004800110412      //---------------------------------------------------------------
004900110412      //?prototipi
005000110412      //---------------------------------------------------------------
005100110412
005200110412      //---------------------------------------------------------------
005300110412      //?M A I N - L I N E
005400110412      //---------------------------------------------------------------
005500110412
005600110412     c     *Entry        plist
005700110412     c                   parm                    kpjba
005800110412
005900110412      /free
006000110412
006100110412       //?Operazioni iniziali
006200110412       exsr RoutInz;
006300110412
006400110412       //?Gestione video
006500110412       DOW $Fine = *off;
006600110412         exsr GesVID;
006700110412       ENDDO;
006800110412
006900110412       //?Operazioni finali
007000110412       exsr RoutEnd;
007100110412
007200110412       //--------------------------------------------------------------
007300110412       //?Operazioni iniziali.
007400110412       //--------------------------------------------------------------
007500110412       BEGSR RoutInz;
007600110412
007700110412       ENDSR;
007800110412
007900110412       //--------------------------------------------------------------
008000110412       //?Gestione videata.
008100110412       //--------------------------------------------------------------
008200110412       BEGSR GesVID;
008300110412
008400110412       //?Emissione videata
008500150528         exfmt  OR01001;
008600110412
008700110412         SELECT;
008800110412
008900110412       //?- F03=Fine
009000110412           WHEN  *INkc;
009100110412             exsr F03vid;
009200110412
009300110412       //?Invio
009400110412           OTHER;
009500110412             exsr enter;
009600110412
009700110412         ENDSL;
009800110412
009900110412       ENDSR;
010000110412
010100110412       //--------------------------------------------------------------
010200110412       //?Gestione tasto funzionale F03 da videata.
010300110412       //?F03=Fine
010400110412       //--------------------------------------------------------------
010500110412       BEGSR F03vid;
010600110412
010700110412         $Fine = *on;
010800110412
010900110412       ENDSR;
011000110412
011100110412       //--------------------------------------------------------------
011200110412       //?Gestione enter.
011300110412       //--------------------------------------------------------------
011400110412       BEGSR Enter;
011500150529
011600150529         clear OOR010poe;
011700150529         clear OOR010nsr;
011800150529         clear OOR010nor;
011900150529         clear OOR010nrv;
012000150529         clear OOR010F03;
012100150529         clear OOR010F12;
012200150529         clear OOR010err;
012300150529         clear OOR010msg;
012400150529         clear FIDNA1DS;
012500150528
012600150528         IF  V01ric = *blanks and V01fieu55 = *blanks and V01ra = *blanks;
012700150528           %subst(kpjbu:1:1) = IOR010ric;
012800150528           FIOR010R (kpjba);
012900150528           leavesr;
013000150528         ENDIF;
013100150528
013200150528         IF  V01fieu55 <> *blanks;
013300150624           %subst(kpjbu:1:1) = 'I';
013400150528           %subst(kpjbu:2:1) = 'D';
013500150528           FIOR010R (kpjba);
013600150528           leavesr;
013700150528         ENDIF;
013800150528
013900150528         IF  V01ric <> *blanks;
014000150624           IOR010ric = '1';
014100150528           FIOR010R (kpjba:FIOR010DS);
014200150528           leavesr;
014300150528         ENDIF;
014400150528
014500150528         IF  V01ra <> *blanks;
014600150528           FIOR010R (kpjba:FIOR010DS:FIDNA1DS);
014700150529           IF  IA1ogg <> *zeros and IA1ogg <> *blanks;
014800150528             OOR010poe = %dec(%subst(IA1ogg:1:3):3:0);
014900150528             OOR010nsr = %dec(%subst(IA1ogg:4:2):2:0);
015000150528             OOR010nor = %dec(%subst(IA1ogg:6:7):7:0);
015100150528             OOR010nrv = %dec(%subst(IA1ogg:13:2):2:0);
015200150528           ENDIF;
015300150528           leavesr;
015400150528         ENDIF;
015500110412
015600110412       ENDSR;
015700110412
015800110412       //--------------------------------------------------------------
015900110412       //?Operazioni finali.
016000110412       //--------------------------------------------------------------
016100110412       BEGSR RoutEnd;
016200110412
016300110412         *inLR = *on;
016400110412         return;
016500110412
016600110412       ENDSR;
016700110412
016800110412      /end-free
