000100100211     ***************************************************************************
000200100211     **
000300150625     ** Wrapper a PJAI1O3/NDC086R.
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
002204150625      *schiera partite in ricerca
002302150625     D SkPartite       DS                  occurs(50)
002303150625     D  SkDtPar                            like(dtpar086)
002304150625     D  SkNrPar                            like(nrpar086)
002305150625     D  SkSerPar                           like(Serpar086)
002306150625     D  SkImportD                    19S 3
002307150625     D  SkDtSca                            like(dtpar086)
002308150625     D  SkSegnoD                      1
002309150625     D  SkImporto                    19S 3
002310150625     D  SkDivisa                           like(Divisa086)
002311150625      *
002312150625     D NDC086DS      E DS
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
003000150625     D                 PR                  EXTPGM('PJCI1O3/NDC086R')
003100100211     D  kpjba                              LIKEDS(kpjba)
003101150625     D  NDC086ds                           LIKEDS(NDC086DS)
003103150625     D  SOC001                             likeDS(XSOC001DS)
003104150625     D  AUTDS                              LIKEDS(XCHKAUTDS)
003105150625     D  skpartite                          like(skpartite)
003106150703
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
003120150626      *
003121150626     D quilngtx        PR                  EXTPGM('QUILNGTX')
003122150626     D  lngTx01                     256A   OPTIONS(*VARSIZE)                    Text string
003123150626     D  lngTx02                      10I 0                                      Length text string
003124150626     D  lngTx03                       7A                                        Message ID
003125150626     D  lngTx04                      20A                                        Qualified MSGF
003126150626     D  lngTx05                      16A   OPTIONS(*VARSIZE)                    Error code
003127150626
003128150625     C     *ENTRY        PLIST
003129150625     C                   PARM                    KPJBA
003130150625     c                   PARM                    ndc086ds
003131150625     C                   PARM                    XSOC001DS
003132150625     C                   PARM                    XCHKAUTDS
003133150625     C                   PARM                    skpartite
006900150625
006911150625      /FREE
006912150625
006913150625           wrk_filiale = %subst(ksc086:2:3);
006914150625
006915151029         if (kcc086 = '000151' or
006916151029            kcc086 = '000400' and %subst(ksc086:2:3)<>'046')
006917151020          and kcdaz <> 'YW02';
006918150703         RESET XAutUniDS ;
006919150703         XAUPrf = KNmUs ;
006920150703         XAUSoc = XscSoc ;
006921150703         XAUUni = wrk_filiale ;
006922150703         AUTUNI(XAUTUNIDS);
006923150703
006924150703         IF XAUAbl = *off ;
006925151029            lngTx01 = 'Utente non autorizzato ai clienti/fornitori +
006926151029               della filiale '
006927151019               + wrk_filiale ;
006928150703               exsr displayLongText;
006929150703               err086 = '1';
006930150703               return;
006931150703         ENDIF;
006932160603
006933160603       else;
006934160603
006935160603         if unita086 = *blanks and xscuff = 'FI';
006936160603            lngTx01 = 'Filiale obbligatoria';
006937160603               exsr displayLongText;
006940160603               err086 = '1';
006941160603               return;
006942160603         endif;
006944160603
006945151012       endif;
006946150703
006948150625           EstrattoConto(KPJBA:ndc086ds:XSOC001DS:XCHKAUTDS:skpartite);
006949150703
009500150625         *INLR = *ON;
009501150625         return;
009502150626
009503150626
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
009600100211
009700100211      /END-FREE
