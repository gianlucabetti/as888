000100110623     ***************************************************************************
000200110623     **
000300110623     ** Il comando RTVDIRINF (Richiamo informazioni indirizzario) viene
000400110623     ** utilizzato per raccogliere attributi per indirizzari e file
000500110623     ** nell'Integrated File System. Le informazioni raccolte sono
000600110623     ** memorizzate nei file di database che sono denominati utilizzando il
000700110623     ** prefisso file di informazioni specificato dal parametro INFFILEPFX.
000800110623     ** I file sono creati nella libreria specificata dal parametro INFLIB.
000900110624     ** Limitazione:
001000110624     ** - Le informazioni sull'indirizzario possono essere richiamate solo
001100110624     **   per file system caricati.
001200110624     ** - Informazioni sull'indirizzario possono essere richiamate solo
001300110624     **   per file system locali.
001400110624     ** - È necessario disporre dell'autorizzazione speciale a tutti gli
001500110624     **   oggetti (*ALLOBJ) per eseguire questo comando, quindi
001600110624     **   QUESTO SERVICE PROGRAM DEVE ESSERE CREATO DA QSECOFR e poi
001700110624     **   CHGSRVPGM SRVPGM(TRULDIRINF) USRPRF(*OWNER)
001800110624     **
001900110623     ***************************************************************************
002000110624     H NOMAIN
002001161011
002002161011     D*--------------------------------------------------------------------
002003161011     D* Apre una directory
002004161011     D*
002005161011     D* opendir(string af a dirname) ==> return memory *address of the directory
002006161011     D*--------------------------------------------------------------------
002007161011     D opendir         PR              *   EXTPROC('opendir')
002008161011     D                                 *   VALUE
002009161011     D*--------------------------------------------------------------------
002012161011     D* Chiude una directory
002013161011     D*
002014161011     D* closedir(string af a dirname) ==> return a value of operation's result
002015161011     D*--------------------------------------------------------------------
002016161011     D closedir        PR            10I 0 EXTPROC('closedir')
002017161011     D                                 *   VALUE
002018161011     D*--------------------------------------------------------------------
002021161011     D* Legge una directory
002022161011     D*
002023161011     D* readdir(*address of a dirname) ==> rtn list main file attrib.into dsinfo
002024161011     D*--------------------------------------------------------------------
002025161011     D readdir         PR              *   EXTPROC('readdir')
002026161011     D                                 *   VALUE
002027161011
002028161011     D*--------------------------------------------------------------------
002029161011     D* Compone e Verifica Regular Expression
002030161011     D*
002031161011     D*--------------------------------------------------------------------
002032161011     D regcomp         PR            10I 0 EXTPROC('regcomp')
002033161011     D   preg                          *   VALUE
002034161011     D   pattern                       *   VALUE
002035161011     D   cflags                      10I 0 VALUE
002036161011     D*
002037161011     D regexec         PR            10I 0 EXTPROC('regexec')
002038161011     D   preg                          *   VALUE
002039161011     D   string                        *   VALUE
002040161011     D   nmatch                      10U 0 VALUE
002041161011     D   pmatch                        *   VALUE
002042161011     D   eflags                      10I 0 VALUE
002043161011     D*
002044161011     D regfree         PR                  EXTPROC('regfree')
002045161011     D   preg                          *   VALUE
002046161011     D*
002047161011     D regex_t         DS                  align
002048161011     D  re_nsub                      10I 0
002049161011     D  re_comp                        *
002050161011     D  re_cflags                    10I 0
002051161011     D  re_erroff                    10I 0
002052161011     D  re_len                       10I 0
002053161011     D  re_ucoll                     10I 0 DIM(2)
002054161011     D  re_lsub                        *   DIM(9)
002055161011     D  re_esub                        *   DIM(9)
002056161011     D  re_map                      256A
002057161011     D  re_shift                      5I 0
002058161011     D  re_dbcs                       5I 0
002059161011     D*
002060161011     D regmatch_t      DS                  occurs(2) align
002061161011     D  rm_so                        10I 0
002062161011     D  rm_ss                         5I 0
002063161011     D  rm_eo                        10I 0
002064161011     D  rm_es                         5I 0
002065161011
002066161011     ***************************************************************************
002300110623     **
002400110623     ** Strutture dati.
002500110623     **
002600110623     ***************************************************************************
002601161011      /COPY QSYSINC/QRPGLESRC,QMHRCVPM
002800110623      /COPY QSYSINC/QRPGLESRC,QUSEC
002900110623     D done            DS                  QUALIFIED INZ
003000110623     D  newRichiamo                    N
003100110624     D  runRichiamo                    N
003101161011     D rcvm0100        DS                  QUALIFIED
004200110623     D  QMHM010001                         LIKE(QMHM010001)
004300110623     D  QMHMD00                      30A
004400110624     D stm             DS                  QUALIFIED INZ
004500110624     D  select                      255A   VARYING
004600110624     D  from                        255A   VARYING
004700110624     D  join                        255A   VARYING
004800110624     D  where                       255A   VARYING
004900110624     D  orderBy                     255A   VARYING
005000110624     D  stm                        1024A   VARYING
005100110623
005200110623     ***************************************************************************
005300110623     **
005400110623     ** Costanti.
005500110623     **
005600110623     ***************************************************************************
005700110623      /COPY GAITRASRC/SRCCONST,TRULDIRINF
005800161011     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')
005801161011     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')
005802161011
006000110623     ***************************************************************************
006100110623     **
006200110623     ** Campi.
006300110623     **
006400110623     ***************************************************************************
006401161011     D pPathOrig       s            120A
006402161011     D pPathCli        s                   like(§AR8PPDFP)
006403161011     D pNomePDF        s             61A
006404161011     D pCheckOnly      s              1A
006405161011     D pFileName       s            256A
006406161011     D pEsito          s              1A
006407161011
006408161011     D dar8§pdf      e ds
006409161011     D wFileName       s            256A   inz(*blanks)
006410161011     D wPathName       s            256A   inz(*blanks)
006411161011     D wContaFile      s              1S 0 inz
006412161011     D wEsito          s              1A   inz
006413161011
006414161011     D wPreg           s               *
006415161011     D wNmatch         s             10U 0 INZ(2)
006416161011     D wPmatch         s               *
006417161011     D wPattern        s            256A
006418161011     D wStringa        s            256A
006419161011     D wDiraddr        s               *
006420161011     D wFiladdr        s               *
006421161011     D wRv             s             10I 0
006422161011
006423161011     D wDirent         ds                  based(wFiladdr)
006424161011     D   d_reserv1                   16A
006425161011     D   d_reserv2                   10U 0
006426161011     D   d_fileno                    10U 0
006427161011     D   d_reclen                    10U 0
006428161011     D   d_reserv3                   10I 0
006429161011     D   d_reserv4                    8A
006430161011     D   d_nlsinfo                   12A
006431161011     D     nls_ccsid                 10I 0 OVERLAY(d_nlsinfo:1)
006432161011     D     nls_cntry                  2A   OVERLAY(d_nlsinfo:5)
006433161011     D     nls_lang                   3A   OVERLAY(d_nlsinfo:7)
006434161011     D     nls_reserv                 3A   OVERLAY(d_nlsinfo:10)
006435161011     D   d_namelen                   10U 0
006436161011     D   d_name                     640A
006437161011
006600110623     ***************************************************************************
006700110623     **
006800110623     ** Prototipi.
006900110623     **
007000110623     ***************************************************************************
007100161011
007200110623      /COPY GAITRASRC/SRCPROTOPR,QMHRCVPM
007300110623      /COPY GAITRASRC/SRCPROTOPR,TRULDIRINF
007400161011
007600110623     P*--------------------------------------------------
007700110623     P* Procedure name: DIRINF_NewRichiamo
007800110623     P* Purpose:        Esegue un nuovo richiamo informazioni indirizzario.
007900110623     P* Returns:        Esito.
008000110623     P* Parameter:      priIndirizzario
008100110623     P* Parameter:      priAlberoSecondarioIndirizzari
008200110623     P* Parameter:      priPrefissoFileInformazioni
008300110623     P* Parameter:      priLibreriaInformazioni
008400110623     P*--------------------------------------------------
008500110623     P DIRINF_NewRichiamo...
008600110623     P                 B                   EXPORT
008700110623     D DIRINF_NewRichiamo...
008800110623     D                 PI            10I 0
008900110623     D  priIndirizzario...                                                      1
009000110623     D                              512A   VARYING
009100110623     D                                     CONST
009200161011
010200110623     D retField        S             10I 0
010300110623
010400161011       IF done.newRichiamo;
010700110624         RETURN DIRINF_ESITO_RICHIAMO_GIA_PREPARATO;
010800110624       ENDIF;
010900161011
011200110624       IF %LEN(priIndirizzario) = *ZERO OR priIndirizzario = *BLANK;
011300110624         RETURN DIRINF_ESITO_PARAMETRO_OBBLIGATORIO;
011400110624       ENDIF;
011500161011
011601161011
013500110624       RESET stm;
013600110623       done.newRichiamo = *ON;
013700110623
013800110623       RETURN retField;
013900110623
014000161011     P DIRINF_NewRichiamo...
014200110623     P                 E
014300110624
014400110624
014500110624     P*--------------------------------------------------
014600110624     P* Procedure name: DIRINF_SetWhereObjectName
014700110624     P* Purpose:        Imposta la WHERE sul nome oggetto.
014800110624     P* Returns:        Esito.
014900110624     P* Parameter:      priWhereObjectNameSearchCondition => WHERE object name
015000110624     P*                 search condition.
015100110624     P*--------------------------------------------------
015200110624     P DIRINF_SetWhereObjectName...
015300110624     P                 B                   EXPORT
015400110624     D DIRINF_SetWhereObjectName...
015500110624     D                 PI            10I 0
015600110624     D  priWhereObjectNameSearchCondition...
015700110624     D                              255A   VARYING
015800110624     D                                     CONST
015900110624
016000110624     D retField        S             10I 0
016100161011
016400110624       IF NOT done.newRichiamo;
016500110624         RETURN DIRINF_ESITO_RICHIAMO_NON_ESEGUITO;
016600110624       ENDIF;
016700110624
016800110624       IF %LEN(priWhereObjectNameSearchCondition) = *ZERO
016900110624       OR priWhereObjectNameSearchCondition = *BLANK;
017000110624         RETURN DIRINF_ESITO_PARAMETRO_OBBLIGATORIO;
017100110624       ENDIF;
017200110624
017300110624       IF %LEN(stm.where) = *ZERO;
017400110624         stm.where = 'WHERE ';
017500110624       ELSE;
017600110624         stm.where = ' AND ';
017700110624       ENDIF;
017800110624
017900110624       stm.where += 'LCASE(RTRIM(CHAR(O.QEZOBJNAM))) '
018000110624                  + priWhereObjectNameSearchCondition;
018100110624
018200110624       RETURN retField;
018300161011
018500110624     P DIRINF_SetWhereObjectName...
018600110624     P                 E
018700110624
018800110624
018900110624     P*--------------------------------------------------
019000110624     P* Procedure name: DIRINF_SetWhereObjectType
019100110624     P* Purpose:        Imposta la WHERE sul tipo oggetto (p.e. *STMF, *DIR)
019200110624     P* Returns:        Esito.
019300110624     P* Parameter:      priWherObjectTypeSearchCondition
019400110624     P*--------------------------------------------------
019500110624     P DIRINF_SetWhereObjectType...
019600110624     P                 B                   EXPORT
019700110624     D DIRINF_SetWhereObjectType...
019800110624     D                 PI            10I 0
019900110624     D  priWhereObjectTypeSearchCondition...
020000110624     D                              255A   VARYING
020100110624     D                                     CONST
020200110624
020300110624     D retField        S             10I 0
020400161011
020700110624       IF NOT done.newRichiamo;
020800110624         RETURN DIRINF_ESITO_RICHIAMO_NON_ESEGUITO;
020900110624       ENDIF;
021000110624
021100110624       IF %LEN(priWhereObjectTypeSearchCondition) = *ZERO
021200110624       OR priWhereObjectTypeSearchCondition = *BLANK;
021300110624         RETURN DIRINF_ESITO_PARAMETRO_OBBLIGATORIO;
021400110624       ENDIF;
021500110624
021600110624       IF %LEN(stm.where) = *ZERO;
021700110624         stm.where = 'WHERE ';
021800110624       ELSE;
021900110624         stm.where = ' AND ';
022000110624       ENDIF;
022100110624
022200110722       stm.where += 'CHAR(O.QEZOBJTYPE) '
022300110722                  + priWhereObjectTypeSearchCondition;
022400110624
022500110624       RETURN retField;
022600161011
022800110624     P DIRINF_SetWhereObjectType...
022900110624     P                 E
023000110624
023001161011     P*--------------------------------------------------
023002161011     P* Procedure name: DIRINF_RunRichiamo
023003161011     P* Purpose:        Esegue un nuovo richiamo informazioni indirizzario.
023004161011     P* Returns:        Esito.
023005161011     P*--------------------------------------------------
023006161011     P DIRINF_RunRichiamo...
023007161011     P                 B                   EXPORT
023008161011     D DIRINF_RunRichiamo...
023009161011     D                 PI            10I 0
023010161011
023011161011     D retField        S             10I 0
023012161011
023013161011       IF NOT done.newRichiamo;
023014161011         RETURN DIRINF_ESITO_RICHIAMO_NON_PREPARATO;
023015161011       ENDIF;
023016161011
023017161011         // nuova gestione
023019161011
023023161011     C* Inizializzo il flag di errore "locale" a OK
023024161011     C                   eval      wEsito = '0'
023025161011     C*
023026161011     C* Inizializzo i parametri di output
023027161011     C                   eval      pFileName = *blanks
023028161011     C                   eval      pEsito    = '2'
023029161011     C*
023030161011     C* Inizializzo il contatore di file che rispondono alla maschera ricevuta
023031161011     C                   eval      wContaFile = *zeros
023032161011     C*
023033161011     C* Apro la directory/maschera-file da ricercare
023034161011     C                   if        pPathCli <> *blanks
023038161011     C                   eval      wPathName =  %trim(pPathOrig) + '/' +
023039161011     C                                          %trim(pPathCli)  + x'00'
023040161011     C                   else
023041161011     C                   eval      wPathName =  %trim(pPathOrig) + x'00'
023042161011     C                   endif
023043161011     C                   eval      wDiraddr  = opendir(%addr(wPathName))
023044161011     C                   if        wDiraddr  = *NULL
023045161011     C*
023046161011     C* Se errore directory/maschera-file => flag errore "locale" a KO
023047161011     C                   eval      wEsito = '1'
023048161011                           RETURN retField;
023049161011     C                   endif
023050161011     C*
023051161011     C* Effettuo le sostituzioni dei caratteri jolly
023052161011     C                   z-add     *zeros        wPos              3 0
023053161011     C                   eval      wPattern = %trim(pNomePDF)
023054161011     C                   eval      wPattern = %xlate(minu:maiu:wPattern)
023055161011     C                   eval      wPos = %scan('*':wPattern:1)
023056161011     C                   dow       wPos > *zero
023057161011     C                   eval      wPattern = %replace('.*':wPattern:wPos:1)
023058161011     C                   eval      wPos = %scan('*':wPattern:wPos+2)
023059161011     C                   enddo
023060161011
023085161011       done.runRichiamo = *ON;
023086161011
023087161011       RETURN retField;
023088161011
023089161011     P DIRINF_RunRichiamo...
023090161011     P                 E
023091161011
039700110623     P*--------------------------------------------------
039800110623     P* Procedure name: DIRINF_EndRichiamo
039900110623     P* Purpose:        Fine richiamo.
040000110623     P* Returns:        Esito.
040100110623     P*--------------------------------------------------
040200110623     P DIRINF_EndRichiamo...
040300110623     P                 B                   EXPORT
040400110623     D DIRINF_EndRichiamo...
040500110623     D                 PI            10I 0
040600110623
040700110623     D retField        S             10I 0
040800161011
041100110624       IF NOT done.newRichiamo;
041200110624         RETURN DIRINF_ESITO_RICHIAMO_NON_PREPARATO;
041300110623       ENDIF;
041400110623
041401161011         // Chiudo la directory/maschera-file da ricercare
041402161011            wRv = closedir(wDiraddr);
041403161011
041500161011       RESET done;
042600161011
042800110623       RETURN retField;
042900161011
043100110623     P DIRINF_EndRichiamo...
043200110623     P                 E
043300110623
043400110623
043500110623     P*--------------------------------------------------
043600110623     P* Procedure name: DIRINF_GetNextObjectName
043700110623     P* Purpose:        Restituisce il prossimo nome oggetto.
043800110623     P* Returns:        Parent directory index, parent directory path e obj...
043900110623     P*                          ect name.
044000110623     P*--------------------------------------------------
044100110623     P DIRINF_GetNextObjectName...
044200110623     P                 B                   EXPORT
044300110623     D DIRINF_GetNextObjectName...
044400110623     D                 PI            10I 0
044500110623     D priIdxDirObj                        LIKEDS(DIRINF_idxDirObj)
044600110623
044700110623     D retField        S             10I 0 STATIC
044800161011
045100110623       CLEAR priIdxDirObj;
045200110623       CLEAR retField;
045300110623
045400110624       IF NOT done.runRichiamo;
045500110623         RETURN DIRINF_ESITO_RICHIAMO_NON_ESEGUITO;
045600110623       ENDIF;
045700110623
045701161011
045702161011     C*
045703161011     C* Se OK procedo con la lettura di tutti i file che corrispondono alla
045704161011     C* maschera file ricevuta in input - nella cartella ricevuta in input
045705161011     C                   eval      wFiladdr = readdir(wDiraddr)
045706161011     C                   dow       wFiladdr <> *NULL
045707161011     C*
045708161011     C* Se già conteggiato + di 1 file che corrisponde al nome => esco
045709161011     C                   if        wContaFile > 1
045710161011     C                   leave
045711161011     C                   endif
045712161011     C*
045713161011     C* Se richiesto solo controllo esistenza e 1 file già trivato => esco
045714161011     C                   if        pCheckOnly = 'Y' and wContaFile = 1
045715161011     C                   leave
045716161011     C                   endif
045717161011     C*
045718161011     C* Reperisco il nome completo del file corrente
045719161011     C                   eval      wFileName = %subst(d_name:1:d_namelen)
045720161011     C*
045721161011     C* Escludo le cartelle inferiore e superiore
045722161011     C                   if        %trim(wFileName) <> '.'  AND
045723161011     C                             %trim(wFileName) <> '..'
045724161011     C                   eval      wStringa = wFileName
045725161011     C                   eval      wStringa = %xlate(minu:maiu:wStringa)
045726161011     C*
045727161011     C* Compile init
045728161011     C                   clear                   regex_t
045729161011     C                   clear                   regmatch_t
045730161011     C     1             occur     regmatch_t
045731161011     C                   eval      wPmatch = %addr(regmatch_t)
045732161011     C                   eval      wPreg = %addr(regex_t)
045733161011     C                   eval      wPattern = %trim(wPattern) + x'00'
045734161011     C                   eval      wStringa = %trim(wStringa) + x'00'
045735161011     C*
045736161011     C* Compile RE
045737161011     C                   eval      wRv = regcomp(wPreg:%addr(wPattern):1)
045738161011     C                   if        wRv = *zeros
045739161011     C*
045740161011     C* Execute RE
045741161011     C                   eval      wRv = regexec(wPreg:%addr(wStringa):
045742161011     C                                           wNmatch:wPmatch: 0)
045743161011     C*
045744161011     C* Se Regular Expression corrente è soddisfatta ...
045745161011     C                   if        wRv = 0
045746161011     C*
045747161011     C* Conteggio oggetti rispondenti alla maschera-file ricevuta
045748161011     C                   eval      wContaFile = wContaFile + 1
045749161011     C*
045750161011     C* Salvo il nome completo
045751161011     C                   eval      pFileName = wFileName
045752161011     C*
045753161011     C                   endif
045754161011     C                   endif
045755161011     C*
045756161011     C* Scarico la Regular Expression corrente
045757161011     C                   callp     regfree(wPreg)
045758161011     C*
045759161011     C                   endif
045760161011     C*
045761161011     C* Procedo con la lettura di tutti i file che corrispondono alla
045762161011     C* maschera file ricevuta in input - nella cartella ricevuta in input
045763161011     C                   eval      wFiladdr = readdir(wDiraddr)
045764161011     C                   enddo
045765161011
045766161011
045767161011
045800110623       EXEC SQL FETCH NEXT FROM idxDirObj INTO :priIdxDirObj;
045900161011
046000110623       SELECT;
046100110623         WHEN sqlCode = DIRINF_ESITO_ROW_NOT_FOUND;
046200110623           retField = sqlCode;
046300110623         WHEN sqlCode < *ZERO;
046400110623           DUMP(A);
046500110623           retField = sqlCode;
046600110623       ENDSL;
046700110623
046800110623       RETURN retField;
046900161011
047100110623     P DIRINF_GetNextObjectName...
047200110623     P                 E
047300110624
