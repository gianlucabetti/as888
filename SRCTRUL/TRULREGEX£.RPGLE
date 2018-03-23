000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200110623
000701161012     D FileName        S            512    varying
000702161012     D Dir             S            512    varying
000800161012     D esito           S             10I 0
001100161013
001101161013     D i               s              3  0
001102161012
001103161012     D wDiraddr        s               *
001104161012     D wFiladdr        s               *
001105161013     D wPathName       s            256A   inz(*blanks)
001106161013     D wFileName       s            256A   inz(*blanks)
001107161013     D wStringa        s            256A
001108161013
001109161013     D wDirent         ds                  based(wFiladdr)
001110161013     D   d_reserv1                   16A
001111161013     D   d_reserv2                   10U 0
001112161013     D   d_fileno                    10U 0
001113161013     D   d_reclen                    10U 0
001114161013     D   d_reserv3                   10I 0
001115161013     D   d_reserv4                    8A
001116161013     D   d_nlsinfo                   12A
001117161013     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
001118161013     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
001119161013     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
001120161013     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
001121161013     D   d_namelen                   10U 0
001122161013     D   d_name                     640A
001123161013
001201161012     D DS_DirQNTC      ds                  qualified
001202161012     D  indirizzario                256
001203161012     D  prefisso                    256
001204161012     D  estensione                  256
001205161012     D  filename                    256    dim(37)
001206161012
001207161012     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
001208161012     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
001209161012
001210161012     D*--------------------------------------------------------------------
001211161012     D* Apre una directory
001212161012     D*
001213161012     D* opendir(string af a dirname) ==> return memory *address of the directory
001214161012     D*--------------------------------------------------------------------
001215161012     D opendir         PR              *   EXTPROC('opendir')
001216161012     D                                 *   VALUE
001217161012     D*--------------------------------------------------------------------
001218161012     D* Chiude una directory
001219161012     D*
001220161012     D* closedir(string af a dirname) ==> return a value of operation's result
001221161012     D*--------------------------------------------------------------------
001222161012     D closedir        PR            10I 0 EXTPROC('closedir')
001223161012     D                                 *   VALUE
001224161012     D*--------------------------------------------------------------------
001225161012     D* Legge una directory
001226161012     D*
001227161012     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
001228161012     D*--------------------------------------------------------------------
001229161012     D readdir         PR              *   EXTPROC('readdir')
001230161012     D                                 *   VALUE
001231161012
001232161012     D*--------------------------------------------------------------------
001233161012     D* Compone e Verifica Regular Expression
001234161012     D*
001235161012     D*--------------------------------------------------------------------
001236161012     D regcomp         PR            10I 0 EXTPROC('regcomp')
001237161012     D   preg                          *   VALUE
001238161012     D   pattern                       *   VALUE
001239161012     D   cflags                      10I 0 VALUE
001240161012     D*
001241161012     D regexec         PR            10I 0 EXTPROC('regexec')
001242161012     D   preg                          *   VALUE
001243161012     D   string                        *   VALUE
001244161012     D   nmatch                      10U 0 VALUE
001245161012     D   pmatch                        *   VALUE
001246161012     D   eflags                      10I 0 VALUE
001247161012     D*
001248161012     D regfree         PR                  EXTPROC('regfree')
001249161012     D   preg                          *   VALUE
001250161012     D*
001251161012
001253161012      /COPY QSYSINC/QRPGLESRC,QUSEC
001254161012
001255161012     C     *ENTRY        PLIST
001256161012     C                   PARM                    DS_DirQNTC
001257161012
001258161012
001500110623       *INLR = *ON;
001600161012       clear  DS_DirQNTC.filename;
001601161012
001602161012        // Apro la directory/maschera-file da ricercare
001603161012                  wPathName =  %trim(DS_DirQNTC.indirizzario) + x'00';
001604161012                  wDiraddr  = opendir(%addr(wPathName));
001605161012
001606161013              dou i=37;
001607161013
001608161013                    wFiladdr = readdir(wDiraddr);
001609161012        // Se errore directory/maschera-file => flag errore "locale" a KO
001610161012                      if wDiraddr  = *NULL;
001611161013                        leave;
001612161013                      endif;
001614161012
001617161012       // Reperisco il nome completo del file corrente
001618161012                     wFileName = %subst(d_name:1:d_namelen);
001619161012
001620161012       // Escludo le cartelle inferiore e superiore
001621161013                   if wFileName = '.'
001622161013                     or wFileName = '..';
001623161012                     iter;
001624161012                   endif;
001625161012
001626161012                   wStringa = %xlate(minu:maiu:wFileName);
001627161012
001628161012       // Se Regular Expression corrente è soddisfatta ...
001629161012                   //      if DIR_regExp(Pattern:wStringa) = 0;
001630161013                   i += 1;
001632161012             DS_DirQNTC.FileName(i) = wFileName;
001633161013
001634161013            enddo;
001635161012
001636161012          // Chiudo la directory/maschera-file da ricercare
001637161012               closedir(wDiraddr);
001638161012
001639161012           Return;
001640161012
