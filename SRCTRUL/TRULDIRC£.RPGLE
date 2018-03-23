000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200110623
000701161012     D FileName        S            512    varying
000702161012     D Dir             S            512    varying
000800161012     D esito           S             10I 0
000801161014     D esitoStat       S             10i 0
000802161014     D esitoRegExp     S             10i 0
001100161013
001101161013     D i               s              3  0
001102161012
001103161012     D wDiraddr        s               *
001104161012     D wFiladdr        s               *
001105161013     D wPathName       s            256A   inz(*blanks)
001106161013     D wFileName       s            256A   inz(*blanks)
001107161013     D wStringa        s          10000    varying
001108161013     D wPattern        s            256    varying
001109161014     D wEntry          s          10000    varying
001110161103     D wObj            s            256    varying
001111161103     D cmd             s           2000    varying
001112161013
001113161013     D wDirent         ds                  based(wFiladdr)
001114161013     D   d_reserv1                   16A
001115161013     D   d_reserv2                   10U 0
001116161013     D   d_fileno                    10U 0
001117161013     D   d_reclen                    10U 0
001118161013     D   d_reserv3                   10I 0
001119161013     D   d_reserv4                    8A
001120161013     D   d_nlsinfo                   12A
001121161013     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
001122161013     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
001123161013     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
001124161013     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
001125161013     D   d_namelen                   10U 0
001126161013     D   d_name                     640A
001127161013
001201161012     D DS_DirQNTC      ds                  qualified
001202161012     D  indirizzario                256
001203161103     D  prefisso                    128
001204161103     D  estensione                  128
001205161103     D  localdir                    256
001206161103     D  filename                    256    dim(37)
001208161012
001209161012     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
001210161012     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
001211161012
001212161012     D*--------------------------------------------------------------------
001213161012     D* Apre una directory
001214161012     D*
001215161012     D* opendir(string af a dirname) ==> return memory *address of the directory
001216161012     D*--------------------------------------------------------------------
001217161012     D opendir         PR              *   EXTPROC('opendir')
001218161012     D                                 *   VALUE
001219161012     D*--------------------------------------------------------------------
001220161012     D* Chiude una directory
001221161012     D*
001222161012     D* closedir(string af a dirname) ==> return a value of operation's result
001223161012     D*--------------------------------------------------------------------
001224161012     D closedir        PR            10I 0 EXTPROC('closedir')
001225161012     D                                 *   VALUE
001226161012     D*--------------------------------------------------------------------
001227161012     D* Legge una directory
001228161012     D*
001229161012     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
001230161012     D*--------------------------------------------------------------------
001231161012     D readdir         PR              *   EXTPROC('readdir')
001232161012     D                                 *   VALUE
001233161014
001253161014      /copy QSYSINC/QRPGLESRC,QUSEC
001254161014      /copy gaitrasrc/srcprotopr,TRULREGEX
001255161014      /copy gaitrasrc/srcprotopr,TRULSTAT
001256161103      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
001257161014
001258161014     D wStatDs         ds                  likeds(trulstatds)
001259161012
001260161012     C     *ENTRY        PLIST
001261161012     C                   PARM                    DS_DirQNTC
001262161012
001263161012
001500110623       *INLR = *ON;
001600161012       clear  DS_DirQNTC.filename;
001601161013
001602161013                   wPattern =  DS_DirQNTC.estensione;
001603161012
001604161012        // Apro la directory/maschera-file da ricercare
001605161012                  wPathName =  %trim(DS_DirQNTC.indirizzario) + x'00';
001606161012                  wDiraddr  = opendir(%addr(wPathName));
001607161012
001608161013              dou i=37;
001609161013
001610161013                    wFiladdr = readdir(wDiraddr);
001611161012        // Se errore directory/maschera-file => flag errore "locale" a KO
001612161013                      if wFiladdr  = *NULL;
001613161013                        leave;
001614161013                      endif;
001615161013
001620161012       // Reperisco il nome completo del file corrente
001621161012                     wFileName = %subst(d_name:1:d_namelen);
001622161012
001623161012       // Escludo le cartelle inferiore e superiore
001624161013                   if wFileName = '.'
001625161013                     or wFileName = '..';
001626161012                     iter;
001627161012                   endif;
001628161012
001629161014       // Se è una directory --> iter
001630161014               wEntry = %trim(DS_DirQNTC.indirizzario) + '/' + wFilename;
001631161014                  TRULstat(wEntry:wStatds:esitoStat);
001632161014                    if esitoStat < 0 or
001633161014                      Esito = -1;
001634161014                       leave;
001635161014                    endif;
001636161102                      if wStatDs.st_objtype <> '*STMF'
001637161102                     and wStatDs.st_objtype <> '*DSTMF';
001638161014                     iter;
001639161014                   endif;
001640161014
001641161012                   wStringa = %xlate(minu:maiu:wFileName);
001642161012
001643161012       // Se Regular Expression corrente è soddisfatta ...
001644161014                    TRULREGEX(wPattern:wStringa:esitoRegExp);
001645161013                   //      if DIR_regExp(Pattern:wStringa) = 0;
001646161013
001647161103                    if esitoRegexp <> 0;
001648161103                      iter;
001649161103                    endif;
001650170307          // Se richiesto sposto in locale !!!
001651170307          if Ds_DirQNTC.LocalDir <> *blanks;
001652170307
001653170307            wObj =  %trim(Ds_DirQNTC.Indirizzario) + '/'  + %trim(wfilename);
001655170307            cmd = 'MOV OBJ('''
001657170307                + %trimr(wObj) + ''') TODIR('''
001658170307                + %trim(Ds_DirQNTC.LocalDir) + ''')';
001659170307                IF ExecuteCommand(cmd) <> *ZERO;
001661161103                   Esito = -1;
001662161103                   leave;
001663161103                ENDIF;
001664170307
001665170307            wObj =  %trim(Ds_DirQNTC.LocalDir)  + '/'  + %trim(wfilename);
001667170307            cmd = 'CHGATR OBJ('''
001668170307                + %trimr(wObj) + ''') ATR(*ALWSAV) VALUE(*YES)'
001669170307                   ;
001670170307                IF ExecuteCommand(cmd) <> *ZERO;
001671170307                   Esito = -1;
001672170307                   leave;
001673170307                ENDIF;
001674170307
001675170307          ENDIF;
001678161103
001679161103                    i += 1;
001680161103                    DS_DirQNTC.FileName(i) = wFileName;
001681161103
001682161013            enddo;
001683161012
001684161012          // Chiudo la directory/maschera-file da ricercare
001685161012               closedir(wDiraddr);
001686161012
001687161012           Return;
001688161012
