000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200170502
000300170502       // scorre una directory del IFS e rende salvabili tutti gli oggetti non directory
000400170502
000500170508       // parametri d'ingrersso
000600170508     D DS_Dir          ds                  qualified
000700170508     D  indirizzario                256
000800170508     D FolderSN        s              1a
000900110623
001000170508     D*--------------------------------------------------------------------
001100170508     D* praticamente me stesso
001200170508     D*--------------------------------------------------------------------
001300170508     D ReadDirIFS...
001400170508     D                 PR
001500170508     D iDS_Dir                             likeds(DS_Dir)
001600170508     D iFolderSN                           like(FolderSN)
001700170508
001800161012
001900170508     C     *ENTRY        PLIST
002000170508     C                   PARM                    DS_Dir
002100170508     C                   PARM                    FolderSN
002200161012
002300161012
002400110623       *INLR = *ON;
002500170508       ReadDirIFS(DS_Dir : FolderSN);
002600170508       return;
002700170508
002800170508       //--------------------------------------------------------------
002900170508       // Procedure name: ReadDirIFS                                  ??
003000170508       // Purpose:        Scorre una directory IFS
003100170508       //--------------------------------------------------------------
003200170508
003300170508     p ReadDirIFS...
003400170508     p                 B                   export
003500170508     D ReadDirIFS...
003600170508     D                 PI
003700170508     D iDS_Dir                             likeds(DS_Dir)
003800170508     D iFolderSN                           like(FolderSN)
003900170508
004000170508        // variabili locali
004100170508
004200170508     D wDS_Dir         DS                  likeds(DS_Dir)
004300170508     D FileName        S            512    varying
004400170508     D Dir             S            512    varying
004500170508     D esito           S             10I 0
004600170508     D esitoStat       S             10i 0
004700170508
004800170508     D i               s              3  0
004900170508
005000170508     D wDiraddr        s               *
005100170508     D wFiladdr        s               *
005200170508     D wPathName       s            256A   inz(*blanks)
005300170508     D wFileName       s            256A   inz(*blanks)
005400170508     D wStringa        s          10000    varying
005500170508     D wPattern        s            256    varying
005600170508     D wEntry          s          10000    varying
005700170508     D wObj            s            256    varying
005800170508     D cmd             s           2000    varying
005900170508
006000170508     D wDirent         ds                  based(wFiladdr)
006100170508     D   d_reserv1                   16A
006200170508     D   d_reserv2                   10U 0
006300170508     D   d_fileno                    10U 0
006400170508     D   d_reclen                    10U 0
006500170508     D   d_reserv3                   10I 0
006600170508     D   d_reserv4                    8A
006700170508     D   d_nlsinfo                   12A
006800170508     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
006900170508     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
007000170508     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
007100170508     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
007200170508     D   d_namelen                   10U 0
007300170508     D   d_name                     640A
007400170508
007500170508     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
007600170508     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
007700170508
007800170508     D*--------------------------------------------------------------------
007900170508     D*
008000170508     D*
008100170508     D* opendir(string af a dirname) ==> return memory *address of the directory
008200170508     D*--------------------------------------------------------------------
008300170508     D opendir         PR              *   EXTPROC('opendir')
008400170508     D                                 *   VALUE
008500170508     D*--------------------------------------------------------------------
008600170508     D* Chiude una directory
008700170508     D*
008800170508     D* closedir(string af a dirname) ==> return a value of operation's result
008900170508     D*--------------------------------------------------------------------
009000170508     D closedir        PR            10I 0 EXTPROC('closedir')
009100170508     D                                 *   VALUE
009200170508     D*--------------------------------------------------------------------
009300170508     D* Legge una directory
009400170508     D*
009500170508     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
009600170508     D*--------------------------------------------------------------------
009700170508     D readdir         PR              *   EXTPROC('readdir')
009800170508     D                                 *   VALUE
009900170508
010000170508      /copy QSYSINC/QRPGLESRC,QUSEC
010100170508      /copy gaitrasrc/srcprotopr,TRULSTAT
010200170508      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
010300170508
010400170508     D wStatDs         ds                  likeds(trulstatds)
010500170508
010600161012
010700161012        // Apro la directory/maschera-file da ricercare
010800170508                  wPathName =  %trim(iDS_Dir.indirizzario) + x'00';
010900161012                  wDiraddr  = opendir(%addr(wPathName));
011000161012
011100170502              dow wDiraddr <> *NULL;
011200161013
011300161013                    wFiladdr = readdir(wDiraddr);
011400161012        // Se errore directory/maschera-file => flag errore "locale" a KO
011500161013                      if wFiladdr  = *NULL;
011600161013                        leave;
011700161013                      endif;
011800161013
011900161012       // Reperisco il nome completo del file corrente
012000161012                     wFileName = %subst(d_name:1:d_namelen);
012100161012
012200161012       // Escludo le cartelle inferiore e superiore
012300161013                   if wFileName = '.'
012400161013                     or wFileName = '..';
012500161012                     iter;
012600161012                   endif;
012700161012
012800161014       // Se è una directory --> iter
012900170508               wEntry = %trim(iDS_Dir.indirizzario) + '/' + wFilename;
013000161014                  TRULstat(wEntry:wStatds:esitoStat);
013100161014                    if esitoStat < 0 or
013200161014                      Esito = -1;
013300161014                       leave;
013400161014                    endif;
013500170508                    // se è stato scelto di scendere nelle directory contenute
013600170508                    if iFolderSN = 'S' and
013700170508                      wStatDs.st_objtype = '*DIR';
013800170508                      // richiamo me stesso passando come indirizzario quello d'ingresso + il nome folder
013900170508                      wDS_Dir.indirizzario =
014000170508                                 %trim(iDS_Dir.indirizzario)
014100170508                                 + '/' + wFilename;
014200170508                      ReadDirIFS(wDS_Dir : iFolderSN);
014300170508                    endif;
014400170508
014500161102                      if wStatDs.st_objtype <> '*STMF'
014600161102                     and wStatDs.st_objtype <> '*DSTMF';
014700161014                     iter;
014800161014                   endif;
014900161014
015000161012                   wStringa = %xlate(minu:maiu:wFileName);
015100170307
015200170508            wObj =  %trim(iDs_Dir.indirizzario)  + '/'  + %trim(wfilename);
015300170307            cmd = 'CHGATR OBJ('''
015400170307                + %trimr(wObj) + ''') ATR(*ALWSAV) VALUE(*YES)'
015500170307                   ;
015600170307                IF ExecuteCommand(cmd) <> *ZERO;
015700170508                // in questo pgm proseguo
015800170508                // Esito = -1;
015900170508                // leave;
016000170307                ENDIF;
016100161103
016200161013            enddo;
016300161012
016400161012          // Chiudo la directory/maschera-file da ricercare
016500161012               closedir(wDiraddr);
016600161012
016700161012           Return;
016800161012
016900170508     P ReadDirIFS...
017000170508     P                 E
