000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200170502
000300170502       // scorre una directory del IFS e rende salvabili tutti gli oggetti non directory
000400170502
000500170508       // parametri d'ingrersso
000600170508     D DS_Dir          ds                  qualified
000700170508     D  indirizzario                256
000800170508     D FolderSN        s              1a
000900110623
001000161012     D FileName        S            512    varying
001100161012     D Dir             S            512    varying
001200161012     D esito           S             10I 0
001300161014     D esitoStat       S             10i 0
001400161013
001500161013     D i               s              3  0
001600161012
001700161012     D wDiraddr        s               *
001800161012     D wFiladdr        s               *
001900161013     D wPathName       s            256A   inz(*blanks)
002000161013     D wFileName       s            256A   inz(*blanks)
002100161013     D wStringa        s          10000    varying
002200161013     D wPattern        s            256    varying
002300161014     D wEntry          s          10000    varying
002400161103     D wObj            s            256    varying
002500161103     D cmd             s           2000    varying
002600170508
002700161013     D wDirent         ds                  based(wFiladdr)
002800161013     D   d_reserv1                   16A
002900161013     D   d_reserv2                   10U 0
003000161013     D   d_fileno                    10U 0
003100161013     D   d_reclen                    10U 0
003200161013     D   d_reserv3                   10I 0
003300161013     D   d_reserv4                    8A
003400161013     D   d_nlsinfo                   12A
003500161013     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
003600161013     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
003700161013     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
003800161013     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
003900161013     D   d_namelen                   10U 0
004000161013     D   d_name                     640A
004100161013
004200161012     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
004300161012     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
004400161012
004500161012     D*--------------------------------------------------------------------
004600170508     D*
004700161012     D*
004800161012     D* opendir(string af a dirname) ==> return memory *address of the directory
004900161012     D*--------------------------------------------------------------------
005000161012     D opendir         PR              *   EXTPROC('opendir')
005100161012     D                                 *   VALUE
005200161012     D*--------------------------------------------------------------------
005300161012     D* Chiude una directory
005400161012     D*
005500161012     D* closedir(string af a dirname) ==> return a value of operation's result
005600161012     D*--------------------------------------------------------------------
005700161012     D closedir        PR            10I 0 EXTPROC('closedir')
005800161012     D                                 *   VALUE
005900161012     D*--------------------------------------------------------------------
006000170508     D* Legge una directory
006100161012     D*
006200161012     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
006300161012     D*--------------------------------------------------------------------
006400161012     D readdir         PR              *   EXTPROC('readdir')
006500161012     D                                 *   VALUE
006600170508     D*--------------------------------------------------------------------
006700170508     D* me stesso
006800170508     D*--------------------------------------------------------------------
006900170508     D*TRULDIRC        PR                  extpgm('TRULDIRC')
007000170508     D TRULDIRC        PR
007100170508     D iDS_Dir                             likeds(DS_Dir)
007200170508     D iFolderSN                           like(FolderSN)
007300170508
007400170508     D TRULDIRC        PI
007500170508     D iDS_Dir                             likeds(DS_Dir)
007600170508     D iFolderSN                           like(FolderSN)
007700161014
007800161014      /copy QSYSINC/QRPGLESRC,QUSEC
007900161014      /copy gaitrasrc/srcprotopr,TRULSTAT
008000161103      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
008100161014
008200161014     D wStatDs         ds                  likeds(trulstatds)
008300161012
008400170508     C*    *ENTRY        PLIST
008500170508     C*                  PARM                    DS_Dir
008600170508     C*                  PARM                    FolderSN
008700161012
008800161012
008900110623       *INLR = *ON;
009000170508       DS_Dir=iDS_Dir;
009100170508       FolderSN=iFolderSN;
009200161012
009300161012        // Apro la directory/maschera-file da ricercare
009400170502                  wPathName =  %trim(DS_Dir.indirizzario) + x'00';
009500161012                  wDiraddr  = opendir(%addr(wPathName));
009600161012
009700170502              dow wDiraddr <> *NULL;
009800161013
009900161013                    wFiladdr = readdir(wDiraddr);
010000161012        // Se errore directory/maschera-file => flag errore "locale" a KO
010100161013                      if wFiladdr  = *NULL;
010200161013                        leave;
010300161013                      endif;
010400161013
010500161012       // Reperisco il nome completo del file corrente
010600161012                     wFileName = %subst(d_name:1:d_namelen);
010700161012
010800161012       // Escludo le cartelle inferiore e superiore
010900161013                   if wFileName = '.'
011000161013                     or wFileName = '..';
011100161012                     iter;
011200161012                   endif;
011300161012
011400161014       // Se è una directory --> iter
011500170502               wEntry = %trim(DS_Dir.indirizzario) + '/' + wFilename;
011600161014                  TRULstat(wEntry:wStatds:esitoStat);
011700161014                    if esitoStat < 0 or
011800161014                      Esito = -1;
011900161014                       leave;
012000161014                    endif;
012100170508                    // se è stato scelto di scendere nelle directory contenute
012200170508                    if FolderSN = 'S' and
012300170508                      wStatDs.st_objtype = '*DIR';
012400170508                      // richiamo me stesso passando come indirizzario quello d'ingresso + il nome folder
012500170508                      iDS_Dir.indirizzario = %trim(DS_Dir.indirizzario)
012600170508                                             + '/' + wFilename;
012700170508                      iFolderSN = FolderSN;
012800170508       //             truldirc(iDS_Dir : iFolderSN);
012900170508     C                   CALL      'TRULDIRC'
013000170508     C                   PARM                    iDS_Dir
013100170508     C                   PARM                    iFolderSN
013200170508                    endif;
013300170508
013400161102                      if wStatDs.st_objtype <> '*STMF'
013500161102                     and wStatDs.st_objtype <> '*DSTMF';
013600161014                     iter;
013700161014                   endif;
013800161014
013900161012                   wStringa = %xlate(minu:maiu:wFileName);
014000170307
014100170502            wObj =  %trim(Ds_Dir.indirizzario)  + '/'  + %trim(wfilename);
014200170307            cmd = 'CHGATR OBJ('''
014300170307                + %trimr(wObj) + ''') ATR(*ALWSAV) VALUE(*YES)'
014400170307                   ;
014500170307                IF ExecuteCommand(cmd) <> *ZERO;
014600170508                // in questo pgm proseguo
014700170508                // Esito = -1;
014800170508                // leave;
014900170307                ENDIF;
015000161103
015100161013            enddo;
015200161012
015300161012          // Chiudo la directory/maschera-file da ricercare
015400161012               closedir(wDiraddr);
015500161012
015600161012           Return;
015700161012
