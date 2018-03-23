000100100211     ***************************************************************************
000200100211     **
000300160603     ** Wrapper a PJCI1O3/NDC089R.
000400100211     **
000500100211     ***************************************************************************
000600100211     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000601150625     H DECEDIT('0,') DATEDIT(*DMY.) DATFMT(*ISO)
000602150625     H BNDDIR('PJXBND')
000603150625
000800100211     ***************************************************************************
000900100211     **
001000100211     ** Costanti.
001100100211     **
001200100211     ***************************************************************************
001300100211     D PROJ_ENTER...
001400100211     D                 C                   ' '
001500100211     D PROJ_F3...
001600100211     D                 C                   '1'
001700100211     D PROJ_F8...
001800100211     D                 C                   '0'
001900100211     D PROJ_F12...
002000100211     D                 C                   '2'
002100100211     D PROJ_OPZIONE_RICERCA...
002200100211     D                 C                   '01'
002201150625      *
002202150625     D conteggio       s              3  0
002203150625     D wrk_filiale     s              3
002204160603      *
002312160603     D NDC089DS      E DS
002313150625      * Reperimento Autorizzazioni
002317150625     D XCHKAUTDS     E DS
002318150625     D* Dati di ambiente ottenuti da XSOC
002319150625     D XSOC001DS     E DS
002320150625     D*
002332150625     D kpjba         e ds
002333150703      *
002334150703     D* Autorizzazioni alla unità
002335150703     D XAutUniDS     E DS
002400100211     ***************************************************************************
002500100211     **
002600100211     ** Prototipi.
002700100211     **
002800100211     ***************************************************************************
002900150625     D EstrattoConto...
003000160603     D                 PR                  EXTPGM('PJCI1O3/NDC089R')
003100100211     D  kpjba                              LIKEDS(kpjba)
003101160603     D  NDC089ds                           LIKEDS(NDC089DS)
003103150625     D  SOC001                             likeDS(XSOC001DS)
003104150625     D  AUTDS                              LIKEDS(XCHKAUTDS)
003105160603
003107150703     D AutUni...
003108150703     D                 PR                  EXTPGM('XAUTUNI')
003109150703     D  AUTUNIDS                           LIKEDS(XAUTUNIDS)
003110150703
003111150626      /COPY QSYSINC/QRPGLESRC,QUSEC
003112150626     D lngTx01         S            256A
003113150626     D lngTx02         S             10I 0
003114150626     D                                     INZ(%SIZE(lngTx01))
003115150626     D lngTx03         S              7A
003116150626     D                                     INZ('BAR0019')
003117150626     D lngTx04         S             20A
003118150626     D                                     INZ('YBARMSG   *LIBL')
003119150626
003120160603     D quilngtx        PR                  EXTPGM('QUILNGTX')
003122150626     D  lngTx01                     256A   OPTIONS(*VARSIZE)                    Text string
003123150626     D  lngTx02                      10I 0                                      Length text string
003124150626     D  lngTx03                       7A                                        Message ID
003125150626     D  lngTx04                      20A                                        Qualified MSGF
003126150626     D  lngTx05                      16A   OPTIONS(*VARSIZE)                    Error code
003127150626
003128150625     C     *ENTRY        PLIST
003129150625     C                   PARM                    KPJBA
003130160603     c                   PARM                    ndc089ds
003131150625     C                   PARM                    XSOC001DS
003132150625     C                   PARM                    XCHKAUTDS
003133160603
006913160603           wrk_filiale = %subst(ksc089:2:3);
006914150625
006915160905        if %trim(kcc089) = 'C' ;
006916160905            kcc089 = '000151';
006917160905        ENDIF;
006918160905        if %trim(kcc089) = 'F' ;
006919160905            kcc089 = '00400';
006920160905        endif;
006921160905
006923160905        if kcdaz <> 'YW02';
006924160905         if kcc089 = '000151' or
006925160905            kcc089 = '000400' ;
006926160905            // and %subst(ksc089:2:3)<>'046';
006927160905
006928150703         RESET XAutUniDS ;
006929150703         XAUPrf = KNmUs ;
006930150703         XAUSoc = XscSoc ;
006931150703         XAUUni = wrk_filiale ;
006932150703         AUTUNI(XAUTUNIDS);
006933150703
006934150703         IF XAUAbl = *off ;
006935151029            lngTx01 = 'Utente non autorizzato ai clienti/fornitori +
006936151029               della filiale '
006937151019               + wrk_filiale ;
006938150703               exsr displayLongText;
006939160603               err089 = '1';
006940150703               return;
006941150703         ENDIF;
006942160603
006943160603       else;
006944160603
006945160603         if unita089 = *blanks and xscuff = 'FI';
006946160606            lngTx01 = 'Per conti diversi da Clienti/Fornitori +
006947160905             utente di filiale ' + knmus +
006948160905              ' deve indicare l''unità'
006949160905                 ;
006950160905
006951160603               exsr displayLongText;
006952160603               err089 = '1';
006953160603               return;
006954160603         endif;
006955160603
006956160905        endif;
006957160905       endif;
006958150703
006959160603           EstrattoConto(KPJBA:ndc089ds:XSOC001DS:XCHKAUTDS);
006960150703
009500150625         *INLR = *ON;
009501150625         return;
009502160603
009504150626       BEGSR displayLongText;
009505150626
009506150626         // lngTx01 = ycow0120s.rtnCode;
009507150626         // lngTx03 = 'BAR0019';
009508150626
009509150626         CLEAR qusec;
009510150626         qusbprv = %SIZE(qusec);
009511150626         quilngtx(lngTx01:lngTx02:lngTx03:lngTx04:qusec);
009512150626
009513150626       ENDSR;
009514150626
