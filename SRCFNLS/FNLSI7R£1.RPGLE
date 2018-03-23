000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400160329      *  Nome programma:  FNLSI7R
000500160329      *  Descrizione   :  Manutenzione Vedi Pacco DPD in partenza
000600160329      *  Data creazione:  29 MAR 2016
000700940712      *------------------------------------------------------------------------*
000800991105     H DECEDIT('0,') DATEDIT(*DMY/)
000900160323     h dftactgrp(*no) actgrp(*caller)
001000940712      *
001100040218     FAZORG01L  IF   E           K DISK
001200160329     Ftabel00f  IF   E           K DISK
001300160329     FFNblp01L  uF   E           K DISK
001400160329     FFiAR401L  IF   E           K DISK
001500160329     FFiARg01L  uF   E           K DISK
001600040218      *------------------------------------------------------------------------*
001700160329     D err             S             78    DIM(05) CTDATA PERRCD(1)
001800991027      *
001900040216     D KPJBA         E DS
002000040225      *
002100160329     D                 DS
002200160329     D  blpAAS                 1      4  0
002300160329     D  blpMGS                 5      8  0
002400160329     D  blpDSP                 1      8  0
002500941014      *
002600160329     d fnlsi7ds      e ds
002700160329     D TIBS02DS      E DS
002800160329     D TIBS34DS      E DS
002900160329     d dDatiute      e ds
003000160329     D fnlv19ds      E DS
003100160329     D tisi95ds      E DS
003200160317     D og143         E DS
003300160329     D DS3A          E DS
003400160329     d dnsd          e ds
003500160329     d dsbl4i        e ds
003600991027      *
003700160329     D fnlv19r         pr                  extpgm('FNLV19R')
003800160329     D  kpjba                              likeds(fnlv19ds)
003900900518     C****************************************************************
004000900518     C*  RIEPILOGO INDICATORI
004100900518     C***************************************************************
004200131111     C* 40-59 - ERRORI
004300920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
004400900518     C*****************************************************************
004500000000     C     *ENTRY        PLIST
004600000000     C                   PARM                    KPJBA
004700160329     C                   PARM                    fnlsi7ds
004800131210     c
004900160316     C                   Z-ADD     1             CODUT             1 0
005000160329     c                   clear                   olsi7err
005100160329     c                   clear                   olsi7msg
005200131210      /free
005300160316
005400160329       if ilsi7tla<>'C' ;
005500160329       //?Cerco la bolla
005600160329          chain (ilsi7aas:ilsi7lnp:ilsi7nrs:ilsi7nsp) fnblp01l;
005700160329          exsr ctrbolla;
005800160329
005900160329       // se non c'è errore --> aggiorno la bolla
006000160329          if olsi7err=' ';
006100160329             exsr Elabora     ;
006200160329          endif    ;
006300160329
006400160329         endif    ;
006500160316
006600160329       if ilsi7tla<>' ' ;
006700160316       eval *inlr=*on;
006800160329       else  ;
006900160329       return  ;
007000160329       endif  ;
007100160316       //--------------------------------------------------------------
007200160316       //?Verfica che la bolla richiesta abbia i requisiti di bolla "VEDI PACCO"
007300160316       //--------------------------------------------------------------
007400160316       BEGSR ctrbolla       ;
007500160329
007600160329       if not %found(fnblp01l);
007700160329             olsi7err='E';
007800160329             olsi7msg=err(02);
007900160316             leavesr ;
008000160316       endif;
008100160329
008200160316       // Deve essere Import DPD
008300160316          clear og143;
008400160329          chain blplnp azorg01l;
008500160316          if %found(azorg01l);
008600160316             og143=orgde3;
008700160316          endif;
008800160316          if §ogntw<>'DPD' ;
008900160329             olsi7err='E';
009000160329             olsi7msg=err(01);
009100160316             leavesr ;
009200160316          endif;
009300160329
009400160316       // Deve avere §b4trd='1'
009500160329       chain (blpaas:blplnp:blpnrs:blpnsp:'I') fiar401l;
009600160316       if %found (fiar401l);
009700160316          dsbl4i=ar4not;
009800160316       endif;
009900160316       if not %found(fiar401l) or §b4trd<>'1';
010000160329             olsi7err='E';
010100160329             olsi7msg=err(01);
010200160317             leavesr ;
010300160316       endif;
010400160329
010500160329       // Deve essere ancora da partire
010600160329       if blpft1<>' '  ;
010700160329             olsi7err='E';
010800160329             olsi7msg=err(03);
010900160329             leavesr ;
011000160329       endif ;
011100160329
011200160316       endsr;
011300160317       //-------------------------------------------------------------*
011400160329       //
011500160317       //-------------------------------------------------------------*
011600160329        Begsr Elabora      ;
011700160329      /end-free
011800160329
011900160329     c* Verifico se passato un indirizzo corretto
012000160329
012100160329     C* CHAIN CON CODICE BOLLA
012200160329     C                   MOVEL     '3A'          COD
012300160329     C                   MOVEL(P)  blpCBO        KEY
012400160329     C     KTAB2         CHAIN     TABEL                              30
012500160329     C  N30TBLFLG        IFNE      ' '
012600160329     C                   SETON                                        30
012700160329     C                   END
012800160329     C  N30              MOVEL     TBLUNI        DS3A
012900160329     C   30              CLEAR                   DS3A
013000160329     C*
013100160329     C                   MOVEL     §3ATB1        FLGTB1            1
013200160317
013300160329     C                   CLEAR                   Tisi95DS
013400160329     C                   MOVEL     '7'           I95TCN
013500160329     C                   MOVEL     ilsi7cad      I95CAP
013600160329     C                   MOVEL     ilsi7lod      I95LOC
013700160329     C                   MOVEL     ilsi7prd      I95PRV
013800160329     C                   MOVEL     ilsi7nzd      I95NAR
013900160329     C                   MOVEL     blpDSP        I95DAT
014000160329     C                   MOVEL     blpTFP        I95TFP
014100160329     C                   Z-ADD     blpPKF        I95LKG
014200160329     C                   Z-ADD     blpVLF        I95LMC
014300160329     C                   MOVEL     blpFFD        I95FFD
014400160329     C                   MOVEL     FLGTB1        I95TPO
014500160329     C                   MOVEL     blpTSP        I95TSP
014600160329     C                   MOVEL     'S'           I95FRE
014700160329     C     §3AFCA        IFNE      0
014800160329     C                   MOVEL     'S'           I95FCA
014900160329     C                   ENDIF
015000160329     C**
015100160329     c* Verifico direttamente col tisi95r
015200160329     C                   CALL      'TISI95R'
015300160329     C                   PARM                    Tisi95DS
015400160329     C*
015500160329     c                   if        o95err<>' '
015600160329     c                   eval      olsi7err=o95err
015700160329     c                   eval      olsi7msg=o95msg
015800160329     c                   else
015900160329     c                   if        o95lia<'2'
016000160329     c                   eval      olsi7err='E'
016100160329     c                   eval      olsi7msg=err(04)
016200160329     c                   endif
016300160329     c                   endif
016400160329     c
016500160329     c* se non ci sono errori --> aggiorno
016600160329     c                   if        olsi7err=' '
016700160329     c                   eval      blprsd=ilsi7rsd
016800160329     c                   eval      blpind=ilsi7ind
016900160329     c                   eval      blpcad=ilsi7cad
017000160329     c                   eval      blplod=ilsi7lod
017100160329     c                   eval      blpPRD=ilsi7PRD
017200160329     c                   eval      blpnzd=ilsi7nzd
017300160329     c                   eval      blpfin=o95iso
017400160329       /free
017500160329        // Aggiorno seconda ragione sociale dest
017600160329              if ilsi7rd2<>*blanks  ;
017700160329                 clear  fnlv19ds ;
017800160329                 D19FTR='T';
017900160329                 D19AAS=blpaas;
018000160329                 D19LNP=blplnp;
018100160329                 D19NRS=blpnrs;
018200160329                 D19NSP=blpnsp;
018300160329                 D19TRC='D';
018400160329                 D19NT1=ilsi7rd2  ;
018500160329                 fnlv19r(fnlv19ds);
018600160329              endif  ;
018700160329
018800160329                 chain (blpaas:blplnp:blpnrs:blpnsp) fiarg01l;
018900160329                 if %found(fiarg01l) and argcgi=§nsdcgi;
019000160329                    clear argcgi;
019100160329                    clear argtgi;
019200160329                    update fiarg000;
019300160329                    else  ;
019400160329                    unlock fiarg01l ;
019500160329                 endif;
019600160329
019700160329        // Aggiorno il flag Traduttore da 1 a 3
019800160329                 clear  fnlv19ds ;
019900160329                 D19FTR='T';
020000160329                 D19AAS=blpaas;
020100160329                 D19LNP=blplnp;
020200160329                 D19NRS=blpnrs;
020300160329                 D19NSP=blpnsp;
020400160329                 D19TRC='I';
020500160329                    §b4trd='3';
020600160329                 D19NT1=dsbl4i;
020700160329                 fnlv19r(fnlv19ds);
020800160329      /end-free
020900160329     c
021000160329     c                   update    fnblp000
021100160329     c                   else
021200160329     c                   unlock    fnblp01l
021300160329     c                   endif
021400160317
021500160329     c                   ENDSR
021600940321      **************************************************************************
021700940713      *     R O U T I N E      I N I Z I A L E
021800940713      **************************************************************************
021900940713     C     *INZSR        BEGSR
022000940713      *
022100160329     C     KTAB2         KLIST
022200160329     C                   KFLD                    CODUT
022300160329     C                   KFLD                    COD               2
022400160329     C                   KFLD                    KEY               8
022500160329
022600160329      * Aggancio tabella NSD (per Import DPD "VEDI PACCO")
022700160329     c                   clear                   dnsd
022800160329     c                   Clear                   Tibs02ds
022900160329     c                   eval      T02tla='L'
023000160329     c                   Eval      T02Mod = 'C'
023100160329     c                   Eval      T02Sif = knsif
023200160329     c                   Eval      T02Cod = 'NSD'
023300160329     c                   Movel(p)  '1'           T02Ke1
023400160329     c                   Call      'TIBS02R'
023500160329     c                   Parm                    Kpjba
023600160329     c                   Parm                    Tibs02ds
023700160329     c                   If        T02Err = *Blanks
023800160329     c                   Movel     T02Uni        dnsd
023900160329     c                   EndIf
024000940713     C                   ENDSR
024100131111
024200940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
024300160329I dati immessi non corrispondono ad una spedizione "VEDI PACCO" DPD            1
024400160329Non trovata spedizione                                                         2
024500160329La bolla è già stata trasmessa in arrivo: non manutenzionabile in partenza     3
024600160329CAP/località/provincia non hanno superato l'AFFIDAILITA' minima per l'aggiorn  4
