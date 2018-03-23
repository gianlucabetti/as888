000100110623     ***************************************************************************
000200161011     **  legge contenuto di una directory
000300161011     ***************************************************************************
002000110624     H NOMAIN
002001161011
002066161011     ***************************************************************************
002300110623     **
002400110623     ** Strutture dati.
002500110623     **
002600110623     ***************************************************************************
002601161011      /COPY QSYSINC/QRPGLESRC,QMHRCVPM
002800110623      /COPY QSYSINC/QRPGLESRC,QUSEC
002900110623     D done            DS                  QUALIFIED INZ
003000110623     D  newRichiamo                    N
003100161012     D rcvm0100        DS                  QUALIFIED
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
005700161011      /COPY GAITRASRC/SRCCONST,TRULDIR
005800161012
006000110623     ***************************************************************************
006100110623     **
006200110623     ** Campi.
006300110623     **
006400110623     ***************************************************************************
006401161013     D pIndirizzario   s            256A
006402161103     D pPrefisso       s            128A
006404161103     D pEstensione     s            128A
006405161012     D pFileName       s            256A
006406161011     D pEsito          s              1A
006407161103     D pLocalDir       s            256A
006408161011
006409161013     D* dar8§pdf      e ds
006410161013
006415161012     D i               s              6  0
006416161012
006417161012     D DS_DirQNTC      ds
006418161012     D  indirizzario                256
006419161103     D  prefisso                    128
006420161103     D  estensione                  128
006421161103     D  LocalDir                    256
006422161012     D  filename                    256    dim(37)
006423161103
006600110623     ***************************************************************************
006700110623     **
006800110623     ** Prototipi.
006900110623     **
007000110623     ***************************************************************************
007100161013
007300161011      /COPY GAITRASRC/SRCPROTOPR,TRULDIR
007400161011
007600110623     P*--------------------------------------------------
007700161011     P* Procedure name: DIR_NewRichiamo
007800110623     P* Purpose:        Esegue un nuovo richiamo informazioni indirizzario.
007900110623     P* Returns:        Esito.
008000110623     P* Parameter:      priIndirizzario
008100110623     P* Parameter:      priAlberoSecondarioIndirizzari
008200110623     P* Parameter:      priPrefissoFileInformazioni
008300110623     P* Parameter:      priLibreriaInformazioni
008400110623     P*--------------------------------------------------
008500161011     P DIR_NewRichiamo...
008600110623     P                 B                   EXPORT
008700161011     D DIR_NewRichiamo...
008800110623     D                 PI            10I 0
008900161012     D  piIndirizzario...
008901161012     D                              512    VARYING
008902161012     D                                     CONST
008903161012     D  piPrefisso...
008904161103     D                              128    VARYING
008905161012     D                                     CONST
008907161012     D  piEstensione...
008908161103     D                              128    VARYING
008909161103     D                                     CONST
008911161103     D  piLocalDir...
008912161103     D                              256    const
009200161011
010200110623     D retField        S             10I 0
010201161012
010202161012
010400161011       IF done.newRichiamo;
010700161011         RETURN DIR_ESITO_RICHIAMO_GIA_PREPARATO;
010800110624       ENDIF;
010900161011
011200161012       IF %LEN(piIndirizzario) = *ZERO OR piIndirizzario = *BLANK;
011300161011         RETURN DIR_ESITO_PARAMETRO_OBBLIGATORIO;
011400110624       ENDIF;
011500161012
011501161012          // Effettuo le sostituzioni dei caratteri jolly
011523161012          // inserire gestione filtri se passati (prefisso e suffisso)
011524161013
011525161013                    pindirizzario = piIndirizzario ;
011526161013                    pprefisso     = piPrefisso ;
011527161013                    pEstensione   = piEstensione ;
011528161103                    pLocalDir     = piLocalDir ;
011529161104                    clear i;
011530161012
011531161012       RESET stm;
013600110623       done.newRichiamo = *ON;
013700110623
013800110623       RETURN retField;
013900110623
014000161011     P DIR_NewRichiamo...
014200110623     P                 E
014300161012
039700110623     P*--------------------------------------------------
039800161011     P* Procedure name: DIR_EndRichiamo
039900110623     P* Purpose:        Fine richiamo.
040000110623     P* Returns:        Esito.
040100110623     P*--------------------------------------------------
040200161011     P DIR_EndRichiamo...
040300110623     P                 B                   EXPORT
040400161011     D DIR_EndRichiamo...
040500110623     D                 PI            10I 0
040600110623
040700110623     D retField        S             10I 0
040800161011
041100110624       IF NOT done.newRichiamo;
041200161011         RETURN DIR_ESITO_RICHIAMO_NON_PREPARATO;
041300161012       ENDIF;
041403161011
041500161011       RESET done;
042600161011
042800110623       RETURN retField;
042900161011
043100161011     P DIR_EndRichiamo...
043200110623     P                 E
043300161012
043500110623     P*--------------------------------------------------
043600161011     P* Procedure name: DIR_GetNextObjectName
043700110623     P* Purpose:        Restituisce il prossimo nome oggetto.
043800110623     P* Returns:        Parent directory index, parent directory path e obj...
043900110623     P*                          ect name.
044000110623     P*--------------------------------------------------
044100161011     P DIR_GetNextObjectName...
044200110623     P                 B                   EXPORT
044300161011     D DIR_GetNextObjectName...
044400110623     D                 PI            10I 0
044500161012     D FileName                     512    varying
044501161012
044700161012     D retField        S             10I 0 STATIC
044703161012
045100161012       CLEAR FileName;
045200110623       CLEAR retField;
045300110623
045400161012       IF NOT done.newRichiamo;
045500161011         RETURN DIR_ESITO_RICHIAMO_NON_ESEGUITO;
045600110623       ENDIF;
045700110623
045701161012                   // leggo QNTC
045702161104               if i = *zero or i = 37;
045703161012                    if DIR_rEADqntc() < 0;
045705161012                       Return -1;
045706161012                    ENDIF;
045707161104                 clear i;
045708161012               endif;
045709161012
045710161012                      i += 1;
045711161012
045721161012                     if Ds_dirQNTC.FileName(i) = *blanks;
045722161012                       Return 100;
045723161012                     ENDIF;
045724161012
045725161012          FileName = Ds_dirQNTC.FileName(i);
045726161012          RETURN retField;
046900161011
047100161011     P DIR_GetNextObjectName...
047200110623     P                 E
047300161013
047375161012     P*--------------------------------------------------
047376161012     P* Procedure name: DIR_ReadQNTC...
047377161012     P* Purpose:
047378161012     P* Returns:        Esito.
047379161012     P*--------------------------------------------------
047380161012     P DIR_ReadQNTC...
047381161012     P                 B                   EXPORT
047382161012     D DIR_ReadQNTC...
047383161103     D                 PI            10I 0
047385161013
047386161013                    DS_dirQNTC.indirizzario = pIndirizzario ;
047387161013                    DS_dirQNTC.prefisso = pPrefisso ;
047388161013                    DS_dirQNTC.Estensione = pEstensione ;
047389161103                    DS_dirQNTC.LocalDir = pLocalDir;
047390161013
047391161012
047399161012     C                   call      'TIS7CALLR'
047400161012     C                   parm      '*LIBL'       lib              10
047401161012     C                   parm      'TRULDIRQ'    obj              10
047402161102     C**                   parm      DS_dirQNTC    params        10240
047403161102     C                   parm                    DS_dirQNTC
047404161012     C                   parm      *blank        esito             1
047405161012
047406161102               //    DS_dirQNTC = params;
047407161013
047409161012                  return 0;
047410161012
047411161012     P DIR_ReadQNTC...
047412161012     P                 E
047413161012
047414161103
