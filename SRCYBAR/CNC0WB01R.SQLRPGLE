000100060321      //**********************************************************************************
000200060321      //
000300060328      // Questo programma immette il dettaglio degli assegni incassati.
000400060321      //
000500060321      //**********************************************************************************
000600060320     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700060320     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000800060320
000900060321      //**********************************************************************************
001000060321      //
001100060321      // Definizione files.
001200060321      //
001300060321      //**********************************************************************************
001400060502     Ffidai03l  uf   E           k disk    prefix(£)
001500060427     Fcnc0wb01d CF   E             WORKSTN
001600060320     F                                     INFDS(infDspF)
001700060320     F                                     INDDS(indDspF)
001800060320     F                                     SFILE(s01:s01Rrn)
001900060320
002000060321      //**********************************************************************************
002100060321      //
002200060321      // Definizione costanti.
002300060321      //
002400060321      //**********************************************************************************
002500060320     D F3              C                   X'33'
002600060320     D F5              C                   X'35'
002700060320     D F6              C                   X'36'
002800060320     D F8              C                   X'38'
002900060320     D F12             C                   X'3C'
003000060322     D F21             C                   X'B9'
003100060320     D Enter           C                   X'F1'
003200060320     D ReverseImage    C                   X'21'
003300060320     D HighIntensity   C                   X'22'
003400060320     D Blink           C                   X'28'
003500060321     D IgnoreDBCSdata...
003600060321     D                 C                   '0'
003700060321     D NeverPromptTheCommand...
003800060321     D                 C                   '0'
003900060321     D CommandRunning...
004000060321     D                 C                   '0'
004100060320
004200060321      //**********************************************************************************
004300060321      //
004400060321      // Definizione delle procedure esterne usate.
004500060321      //
004600060321      //**********************************************************************************
004700060427     D cnc0wb02r       PR
004800060427     D                                     EXTPGM('CNC0WB02R')
004900060428     D  £fle                          3s 0
005000060428     D  £tpi                               like(asstpi)
005100060428     D  £tpa                               like(asstpa)
005200060428     D  £dte                               like(assdte)
005300111007     D  £nra                               like(assnra)
005400060428     D  £abi                               like(assabi)
005500060428     D  £cab                               like(asscab)
005600060428     D  £div                               like(assvca)
005700060428     D  £cas                               like(asscas)
005800111007     D  £file                         1
005900060427     D tibs34r         PR
006000060427     D                                     EXTPGM('TIBS34R')
006100060322     D  tibs34ds
006200060322     D                                     LIKEDS(tibs34ds)
006300060322     D trul29c         PR
006400060320     D                                     EXTPGM('TRUL29C')
006500060320     D  obj                          10A
006600060320     D                                     CONST
006700060320     D  objType                       8A
006800060320     D                                     CONST
006900060320     D  rtnLib                       10A
007000060321     D ProcessCommands...
007100060321     D                 PR
007200060320     D                                     EXTPGM('QCAPCMD')
007300060320     D  sourceCommandString...
007400060321     D                            32767A
007500060321     D                                     CONST
007600060321     D                                     OPTIONS(*VARSIZE)
007700060320     D  sourceCommandStringLength...
007800060320     D                               10I 0
007900060320     D                                     CONST
008000060320     D  optionsControlBlock...
008100060321     D                                     LIKE(QCAP0100)
008200060321     D                                     CONST
008300060321     D                                     OPTIONS(*VARSIZE)
008400060320     D  optionsControlBlockLength...
008500060320     D                               10I 0
008600060320     D                                     CONST
008700060320     D  optionsControlBlockFormat...
008800060320     D                                8A
008900060320     D                                     CONST
009000060320     D  changedCommandString...
009100060321     D                            32767A
009200060321     D                                     OPTIONS(*VARSIZE:*OMIT)
009300060320     D  lengthAvailableForChangedCommandString...
009400060320     D                               10I 0
009500060320     D                                     CONST
009600060320     D  lengthOfChangedCommandStringAvailableToReturn...
009700060320     D                               10I 0
009800060320     D  errorCode...
009900060321     D                                     LIKE(qusec)
010000060321     D                                     OPTIONS(*VARSIZE)
010100060321     D DisplayLongText...
010200060321     D                 PR
010300060321     D                                     EXTPGM('QUILNGTX')
010400060321     D  TextString...
010500060321     D                              256A
010600060321     D                                     CONST
010700060321     D                                     OPTIONS(*VARSIZE)
010800060321     D  LengthOfTextString...
010900060321     D                               10I 0
011000060321     D                                     CONST
011100060321     D  MessageID...
011200060321     D                                7A
011300060321     D                                     CONST
011400060321     D  QualifiedMessageFileName...
011500060321     D                               20A
011600060321     D                                     CONST
011700060321     D  ErrorCode...
011800060321     D                                     LIKE(qusec)
011900060321     D                                     OPTIONS(*VARSIZE)
012000060323
012100060321      //**********************************************************************************
012200060321      //
012300060321      // Definizione strutture dati.
012400060321      //
012500060321      //**********************************************************************************
012600060320     D infDspF         DS
012700060320     D  dsp_aid              369    369A                                        AID byte
012800060320     D  sf_rrn               376    377I 0                                      Subfile rr
012900060320     D indDspF         DS
013000060320     D  sflDspCtl             25     25N
013100060320     D  sflDsp                26     26N
013200060320     D  sflClr                27     27N
013300060320     D  sflEnd                28     28N
013400060321     D  sflNxtChg             29     29N
013500060321     D  errQuadratura         30     30N
013600060321     D  errGenerico           99     99N
013700060320     D cndizion      E DS
013800060320     D                                     BASED(nullPtr)
013900060320     D                                     PREFIX(diz)
014000060320     D andiz00f      E DS
014100060320     D                                     BASED(nullPtr)
014200060320     D                                     PREFIX(pjz)
014300060320     D kpjba         E DS
014400060328     D cnc0wb00s     E DS
014500060320     D                                     INZ
014600060321     D                                     PREFIX(prm)
014700060322     D tibs34ds      E DS
014800060322     D                                     INZ
014900060322
015000060321      //**********************************************************************************
015100060321      //
015200060321      // Definizione variabili.
015300060321      //
015400060321      //**********************************************************************************
015500060322     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
015600060322     D/COPY QSYSINC/QRPGLESRC,QUSEC
015700060320     D nullPtr         S               *
015800060320     D rtnLib          S             10A
015900060320     D opCode          S             10A
016000060320     D                                     INZ('INZC01')
016100060320     D s01Rrn          S              5I 0
016200060320     D esito           S             10I 0
016300060320     D comando         S            256A
016400060321     D testo           S            256A
016500060323     D stampare        S              1N
016600060328     D dtaiso          S               d
016700060328     D dtaeur          S               d   datfmt(*eur)
016800060428     D £tpi            S                   like(asstpi)
016900060428     D £tpa            S                   like(asstpa)
017000060427     D £dte            S                   like(assdte)
017100111007     D £nra            S                   like(assnra)
017200060427     D £abi            S                   like(assabi)
017300060427     D £cab            S                   like(asscab)
017400060427     D £knraz          S                   like(daiknraz)
017500060427     D somma           S                   like(asscas)
017600111007     D £file           S              1
017700060321
017800060322      //**********************************************************************************
017900060322      //
018000060322      // Definizione aree dati.
018100060322      //
018200060322      //**********************************************************************************
018300060427     D cnassds       E DS
018400060427     D                                     EXTNAME(cnass00f)
018500060427     D fidaids       E DS
018600060427     D                                     EXTNAME(fidai00f)
018700060322     D §azute        E DS
018800060322     D                                     EXTNAME(azute00f)
018900060322     D                                     DTAARA
019000060322     D §DatiUte      E DS
019100060322     D                                     EXTNAME(dDatiUte)
019200060322     D                                     DTAARA
019300060328     D xsrda8          PR
019400060328     D                                     EXTPGM('XSRDA8')
019500060328     D  wlbda8
019600060328     D                                     LIKEDS(wlbda8)
019700060328     D wlbda8          DS
019800060328     D                                     INZ
019900060328     D  g08Dat                        8S 0
020000060328     D  g08Inv                        8S 0
020100060328     D  g08Err                        1A
020200060328     D  g08tgi                        5S 0
020300060322
020400060321      //**********************************************************************************
020500060321      //
020600060321      // Definizione parametri procedura.
020700060321      //
020800060321      //**********************************************************************************
020900060320     C     *ENTRY        PLIST
021000060320     C                   PARM                    kpjba
021100060329     C     Kdai          KLIST
021200060329     C                   KFLD                    knraz
021300060329     C                   KFLD                    dteh
021400111007     C                   KFLD                    nrah
021500060329     C                   KFLD                    abiv
021600060329     C                   KFLD                    cabv
021700060428     C                   KFLD                    tpiv
021800060428     C                   KFLD                    tpav
021900060320
022000060320      /FREE
022100060320
022200060320       EXSR inzProc;
022300060320
022400060320       DOU opCode = 'END';
022500060320         SELECT;
022600060320           WHEN opCode = 'END';
022700060320             LEAVE;
022800060320           WHEN opCode = 'INZC01';
022900060320             EXSR inzC01;
023000060320           WHEN opCode = 'PUTC01';
023100060320             EXSR putC01;
023200060320           WHEN opCode = 'CHKC01';
023300060320             EXSR getC01Chk;
023400060323           WHEN opCode = 'PRTDATA';
023500060323             EXSR prtData;
023600060321           WHEN opCode = 'UPDDB';
023700060321             EXSR getC01UpdDb;
023800060321           WHEN opCode = 'ERRORE';
023900060321             EXSR errore;
024000060320           OTHER;
024100060321             prmOpCode = opCode;
024200060320             LEAVE;
024300060320         ENDSL;
024400060320       ENDDO;
024500060320
024600060320       EXSR uscita;
024700060320
024800060320       //*********************************************************************************
024900060320       //
025000060320       // Operazioni iniziali da eseguire una tantum.
025100060320       //
025200060320       //*********************************************************************************
025300060320       BEGSR *INZSR;
025400060321      /END-FREE
025500060321     C/EXEC SQL
025600060321     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
025700060321     C/END-EXEC
025800060321      /FREE
025900060322
026000060322         IN(E) §azute;
026100060322         IF NOT %ERROR;
026200060322           IN(E) §DatiUte;
026300060322         ENDIF;
026400060322         IF %ERROR OR rsut = ' ';
026500060322           tibs34r(tibs34ds);
026600060322           IN §azute;
026700060322           IN §DatiUte;
026800060322         ENDIF;
026900060322
027000060322       ENDSR;
027100060320
027200060320       //*********************************************************************************
027300060320       //
027400060320       // Operazioni iniziali.
027500060320       //
027600060320       //*********************************************************************************
027700060320       BEGSR inzProc;
027800060320
027900060321         RESET opCode;
028000060323         RESET stampare;
028100060320         EXSR chkParm;
028200060320
028300060328         // Duplico FIDaI00F in QTEMP.
028400060328         trul29c('FIDAI00F':'*FILE':rtnLib);
028500060321         SELECT;
028600060321           WHEN rtnLib = ' ';
028700060321             prmRtnCode = 'ERRFIDCI';
028800060321             esito = 1;
028900060321             opCode = 'ERRORE';
029000060321           WHEN rtnLib <> 'QTEMP';
029100060321             CLEAR qcap0100;
029200060321             qcabcsdh = IgnoreDBCSdata;
029300060321             qcapa = NeverPromptTheCommand;
029400060321             qcacmdss = CommandRunning;
029500060321             qcaerved = X'000000000000000000';
029600060328             comando = 'CRTDUPOBJ OBJ(FIDAI*) FROMLIB(' + %TRIMR(rtnLib)
029700060321             + ') OBJTYPE(*FILE) TOLIB(QTEMP)';
029800060321             CLEAR qusec;
029900060321             qusbprv = %SIZE(qusec);
030000060321             ProcessCommands(comando:%SIZE(comando):qcap0100:%SIZE(qcap0100)
030100060321             :'CPOP0100':*OMIT:%INT(0):esito:qusec);
030200060321             IF qusei <> ' ';
030300060321               prmRtnCode = qusei;
030400060321               esito = 2;
030500060321               opCode = 'ERRORE';
030600060321             ENDIF;
030700060321         ENDSL;
030800060320
030900060320       ENDSR;
031000060320
031100060320       //*********************************************************************************
031200060320       //
031300060320       // Controllo dei parametri ricevuti.
031400060320       //
031500060320       //*********************************************************************************
031600060320       BEGSR chkParm;
031700060320
031800060328         cnc0wb00s = kpjbu;
031900060320         CLEAR prmRtnCmd;
032000060320         CLEAR prmRtnCode;
032100060321         CLEAR prmImpImmesso;
032200120118         CLEAR prmnraImmesso;
032300060320
032400060320       ENDSR;
032500060320
032600060320       //*********************************************************************************
032700060320       //
032800060428       // Inizializzo subfile assegni.
032900060320       //
033000060320       //*********************************************************************************
033100060320       BEGSR inzC01;
033200060320
033300060321         opCode = 'PUTC01';
033400060320
033500060321         sflDspCtl = *OFF;
033600060320         sflDsp = *OFF;
033700060320         sflEnd = *OFF;
033800060320         sflClr = *ON;
033900060320         WRITE c01;
034000060321         sflDspCtl = *ON;
034100060320         sflDsp = *ON;
034200060320         sflClr = *OFF;
034300060320         sflNxtChg = *OFF;
034400060320         CLEAR s01Rrn;
034500060320         EXSR azzeraTotali;
034600060320
034700060328         IF prmdivisa = 'EUR';
034800060321      /END-FREE
034900060330     c*  leggo gli assegni mitt. o Bart. non ancora versabili presenti
035000060428     c*   nel file FNARB/FIINC con divisa EUR
035100060321     C/EXEC SQL
035200060428     C+ DECLARE CONTANTE CURSOR FOR
035300111007     c* la tabella CNASS legge solo assegni unici (una sped. e un assegno)
035400111007     c* oppure somma lo stesso assegno perchè paga più spedizioni
035500120118     c* quindi esclude quelli con nra = legame e altri dati assegno vuoti
035600111007     c+ WITH
035700111007     c+ CNASS (tpi, tpa, nra, dte, abi, cab, CAS) AS (
035800111007     c+   select asstpi, asstpa, assnra, assdte, assabi, asscab, asscas
035900111007     c+   from (select asstpi, asstpa, assnra, assdte, assabi, asscab,
036000111007     c+   sum(asscas) asscas
036100111007     c+   from (SELECT asstpi, asstpa, assnra, assdte, assabi, asscab,
036200111007     c+         asscas
036300111007     c+         FROM cnass00F join fnarb00f on assaas = arbaas and
036400111007     c+         arblnp=asslnp and arbnrs=assnrs and arbnsp=assnsp
036500111007     c+         WHERE
036600120118     c+         assabi <> 0 and
036700111007     C+         asstpi = :prmtpiass and assfle = :prmfle and
036800111007     C+         assvca = :prmdivisa and assfve = ' ' and arbcuc = :dutcou
036900111007     C+         and arbnmi = 0 and arbipp=:prmfle
037000111007     c* unisce gli assegni incassati dai prepagati da chiusura operatore
037100111007     c* oppure da fuori
037200111007     c+         UNION ALL
037300111007     c+         SELECT asstpi, asstpa, assnra, assdte, assabi, asscab,
037400111007     c+         asscas
037500111007     c+         FROM cnass00F join fiinc00f on assaas = incaas and
037600111007     c+         inclnp=asslnp and incnrs=assnrs and incnsp=assnsp
037700111007     c+         WHERE
037800120118     c+         assabi <> 0 and
037900111007     C+         asstpi = :prmtpiass and assfle = :prmfle and
038000111007     C+         assvca = :prmdivisa and assfve = ' ' and inccuc = :dutcou
038100111007     C+         and incnmi = 0 and incfle=:prmfle
038200111007     c+         ) as assegni
038300111007     c+   group by asstpi, asstpa, assnra, assdte, assabi, asscab
038400111007     c+   ) as assegni1
038500111007     c+ ),
038600120118     c* la tabella TNCSM legge gli enne assegni incassati a fronte di
038700120118     c* enne spedizioni
038800111007     c*
038900120118     c+ TNCSM (tpi, tpa, nra, DTE, abi, cab, CAS) AS (
039000120118     c+   select csmtpi, csmtpa, csmnra, csmdte, csmabi, csmcab, csmidc
039100111007     c+   FROM cnass00F
039200111007     c+   join fnarb00f on assaas = arbaas and arblnp=asslnp and
039300111007     c+   arbnrs=assnrs and arbnsp=assnsp
039400120118     c+   join tncsm00f on assnra = csmkey
039500111007     c+   WHERE
039600120118     c+   assnra <> ' ' and assabi = 0 and
039700111007     C+   asstpi = :prmtpiass and assfle = :prmfle and
039800111007     C+   assvca = :prmdivisa and assfve = ' ' and arbcuc = :dutcou
039900111007     C+   and arbnmi = 0 and arbipp=:prmfle
040000111007     c+   )
040100111007     c+ SELECT
040200111007     c+ 'A', tpi, tpa, nra, DTE, abi, cab, CAS, ifnull(daiknraz, 0)
040300111007     c+ FROM CNASS
040400111007     C+ left outer join fidai00f on
040500111007     C+ :knraz = DaIKNRAZ and
040600111007     C+ tpa = daitpa and
040700111007     C+ tpi = daitpi and
040800111007     C+ dte = daidte and
040900111007     C+ nra = dainra and
041000111007     C+ abi = daiabi and
041100111007     C+ cab = daicab
041200111007     c+ UNION
041300111007     c+ SELECT
041400111007     c+ 'C', tpi, tpa, nra, DTE, abi, cab, CAS, ifnull(daiknraz, 0)
041500120118     c+ FROM TNCSM
041600111007     C+ left outer join fidai00f on
041700111007     C+ :knraz = DaIKNRAZ and
041800111007     C+ tpa = daitpa and
041900111007     C+ tpi = daitpi and
042000111007     C+ dte = daidte and
042100111007     C+ nra = dainra and
042200111007     C+ abi = daiabi and
042300111007     C+ cab = daicab
042400111007     c+ ORDER BY CAS
042500060328     C/END-EXEC
042600060328     C/EXEC SQL
042700060328     C+ OPEN CONTANTE
042800060321     C/END-EXEC
042900060328      /FREE
043000060328        else;
043100060328      /END-FREE
043200060330     c*   leggo gli assegni mitt. o Bart. non ancora versabili presenti
043300060428     c*   nel file FNARB/FIINC con divisa <> EUR
043400060328     C/EXEC SQL
043500060428     C+ DECLARE CONTANTE1 CURSOR FOR
043600111007     c* la tabella CNASS legge solo assegni unici (una sped. e un assegno)
043700111007     c* oppure somma lo stesso assegno perchè paga più spedizioni
043800120118     c* quindi esclude quelli con nra = legame e altri dati assegno vuoti
043900111007     c+ WITH
044000111007     c+ CNASS (tpi, tpa, nra, dte, abi, cab, CAS) AS (
044100111007     c+   select asstpi, asstpa, assnra, assdte, assabi, asscab, asscas
044200111007     c+   from (select asstpi, asstpa, assnra, assdte, assabi, asscab,
044300111007     c+   sum(asscas) asscas
044400111007     c+   from (SELECT asstpi, asstpa, assnra, assdte, assabi, asscab,
044500111007     c+         asscas
044600111007     c+         FROM cnass00F join fnarb00f on assaas = arbaas and
044700111007     c+         arblnp=asslnp and arbnrs=assnrs and arbnsp=assnsp
044800111007     c+         WHERE
044900120118     c+         assabi <> 0  and
045000111007     C+         assvca <> :prmdivisa and assvca <> 'EUR' and
045100111007     C+         asstpi = :prmtpiass and assfle = :prmfle and
045200130402     C+         assfve = ' ' and arbcuc = :dutcou
045300111007     C+         and arbnmi = 0 and arbipp=:prmfle
045400111007     c* unisce gli assegni incassati dai prepagati da chiusura operatore
045500111007     c* oppure da fuori
045600111007     c+         UNION ALL
045700111007     c+         SELECT asstpi, asstpa, assnra, assdte, assabi, asscab,
045800111007     c+         asscas
045900111007     c+         FROM cnass00F join fiinc00f on assaas = incaas and
046000111007     c+         inclnp=asslnp and incnrs=assnrs and incnsp=assnsp
046100111007     c+         WHERE
046200120118     c+         assabi <> 0 and
046300111007     C+         assvca <> :prmdivisa and assvca <> 'EUR' and
046400111007     C+         asstpi = :prmtpiass and assfle = :prmfle and
046500130402     C+         assfve = ' ' and inccuc = :dutcou
046600111007     C+         and incnmi = 0 and incfle=:prmfle
046700111007     c+         ) as assegni
046800111007     c+   group by asstpi, asstpa, assnra, assdte, assabi, asscab
046900111007     c+   ) as assegni1
047000111007     c+ ),
047100120118     c* la tabella TNCSM legge gli enne assegni incassati a fronte di
047200120118     c* enne spedizioni
047300120118     c+ TNCSM (tpi, tpa, nra, DTE, abi, cab, CAS) AS (
047400120118     c+   select csmtpi, csmtpa, csmnra, csmdte, csmabi, csmcab, csmidc
047500111007     c+   FROM cnass00F
047600111007     c+   join fnarb00f on assaas = arbaas and arblnp=asslnp and
047700111007     c+   arbnrs=assnrs and arbnsp=assnsp
047800120118     c+   join tncsm00f on assnra = csmkey
047900111007     c+   WHERE
048000120118     c+   assnra <> ' ' and assabi = 0 and
048100111007     C+   assvca <> :prmdivisa and assvca <> 'EUR' and
048200111007     C+   asstpi = :prmtpiass and assfle = :prmfle and
048300130402     C+   assfve = ' ' and arbcuc = :dutcou
048400111007     C+   and arbnmi = 0 and arbipp=:prmfle
048500111007     c+   )
048600111007     c+ SELECT
048700111007     c+ 'A', tpi, tpa, nra, DTE, abi, cab, CAS,
048800111007     C+ ifnull(daiknraz, 0)
048900111007     c+ FROM CNASS
049000111007     C+ left outer join fidai00f on :knraz = DaIKNRAZ and
049100111007     C+ tpa = daitpa and
049200111007     C+ tpi = daitpi and
049300111007     C+ dte = daidte and
049400111007     C+ nra = dainra and
049500111007     C+ abi = daiabi and
049600111007     C+ cab = daicab
049700111007     c+ UNION
049800111007     c+ SELECT
049900111007     c+ 'C', tpi, tpa, nra, DTE, abi, cab, CAS,
050000111007     C+ ifnull(daiknraz, 0)
050100120118     c+ FROM TNCSM
050200111007     C+ left outer join fidai00f on :knraz = DaIKNRAZ and
050300111007     C+ tpa = daitpa and
050400111007     C+ tpi = daitpi and
050500111007     C+ dte = daidte and
050600111007     C+ nra = dainra and
050700111007     C+ abi = daiabi and
050800111007     C+ cab = daicab
050900111007     c+ ORDER BY CAS
051000111007     C/END-EXEC
051100060328     C/EXEC SQL
051200060328     C+ OPEN CONTANTE1
051300060328     C/END-EXEC
051400060328      /FREE
051500060328        endif;
051600060321
051700060321         IF sqlCod < 0;
051800060321           prmRtnCode = 'ERRSQLOPN';
051900060321           esito = sqlCod;
052000060321           opCode = 'ERRORE';
052100060321           LEAVESR;
052200060321         ENDIF;
052300060321
052400060321         DOU sqlCod = 100;
052500060328         IF prmdivisa = 'EUR';
052600060321      /END-FREE
052700060321     C/EXEC SQL
052800111007     C+ FETCH NEXT FROM CONTANTE INTO :£file, :£tpi, :£tpa, :£nra, :£dte,
052900110812     C+ :£abi, :£cab, :somma, :£knraz
053000060321     C/END-EXEC
053100060321      /FREE
053200060328       else;
053300060328      /END-FREE
053400060328     C/EXEC SQL
053500111007     C+ FETCH NEXT FROM CONTANTE1 INTO :£file, :£tpi, :£tpa, :£nra, :£dte,
053600110812     C+ :£abi, :£cab, :somma, :£knraz
053700060428     C/END-EXEC
053800060328      /FREE
053900060328       endif;
054000060321           SELECT;
054100060321             WHEN sqlCod = 100;
054200060321               sflEnd = *ON;
054300060321               LEAVE;
054400060321             WHEN sqlCod < 0;
054500060321               prmRtnCode = 'ERRSQLFET';
054600060321               esito = sqlCod;
054700060321               opCode = 'ERRORE';
054800060321               LEAVE;
054900060321             OTHER;
055000060321               EXSR setS01;
055100060328               sflNxtChg = (selv = '1');
055200060321               s01Rrn += 1;
055300060321               WRITE s01;
055400060321               EXSR calcolaTotali;
055500060321           ENDSL;
055600060321         ENDDO;
055700060321
055800060328         IF prmdivisa = 'EUR';
055900060321      /END-FREE
056000060321     C/EXEC SQL
056100060321     C+ CLOSE CONTANTE
056200060321     C/END-EXEC
056300060321      /FREE
056400060328       else;
056500060328      /END-FREE
056600060328     C/EXEC SQL
056700060328     C+ CLOSE CONTANTE1
056800060328     C/END-EXEC
056900060328      /FREE
057000060330       endif;
057100060320       ENDSR;
057200060321
057300060321       //*********************************************************************************
057400060321       //
057500060428       // Impostazione riga assegno.
057600060321       //
057700060321       //*********************************************************************************
057800060321       BEGSR setS01;
057900060321
058000060427        if £knraz <> 0;
058100060328         selv = '1';
058200060328        else;
058300060328         selv = ' ';
058400060328        endif;
058500060427         dteh = £dte;
058600060428         tpav = £tpa;
058700111007         fileh = £file;
058800111007         nrah = £nra;
058900111007         nrav = £nra;
059000060427         tpiv = prmtpiass;
059100060427         abiv = £abi;
059200060427         cabv = £cab;
059300060328           RESET wlbda8;
059400060427           g08inv = £dte;
059500060428           g08err = '3';
059600060328           xsrda8(wlbda8);
059700060328           IF g08Err = '0';
059800060427             Dtev= g08dat;
059900060328           ENDIF;
060000060427         vcav = prmdivisa;
060100060427         casv = somma;
060200060428         cash = somma;
060300060321
060400060321       ENDSR;
060500060320
060600060320       //*********************************************************************************
060700060320       //
060800060428       // Emissione subfile assegni.
060900060320       //
061000060320       //*********************************************************************************
061100060320       BEGSR putC01;
061200060320
061300060321         opCode = 'PUTC01';
061400060330
061500060330         c01Div = prmDivisa;
061600060330         IF prmImpTotale > 0;
061700060330           c01Importo = prmImpTotale;
061800060330         ELSE;
061900060330           c01Importo = prmImpImmesso;
062000060330         ENDIF;
062100060321
062200060320         WRITE f01;
062300060320         EXFMT c01;
062400060320
062500060320         prmRtnCmd = dsp_aid;
062600060320
062700060320         SELECT;
062800060320           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
062900060320             opCode = 'END';
063000060320           WHEN dsp_aid = F5;
063100060320             opCode = 'INZC01';
063200060320           WHEN dsp_aid = F6 OR dsp_aid = Enter;
063300060320             opCode = 'CHKC01';
063400060322           WHEN dsp_aid = F21;
063500060323             stampare = *ON;
063600060320         ENDSL;
063700060320
063800060320       ENDSR;
063900060320
064000060320       //*********************************************************************************
064100060320       //
064200060320       // Lettura subfile partite incassate per controlli.
064300060320       //
064400060320       //*********************************************************************************
064500060320       BEGSR getC01Chk;
064600060320
064700060321         opCode = 'PUTC01';
064800060320         *IN99 = *OFF;
064900060321         CLEAR p01Importo;
065000060321         CLEAR p01MsgId30;
065100060320         EXSR azzeraTotali;
065200060321         sflNxtChg = *ON;
065300060320
065400060320         DOU %EOF;
065500060320           READC s01;
065600060320           IF %EOF;
065700060320             LEAVE;
065800060320           ENDIF;
065900060320           EXSR calcolaTotali;
066000060320           UPDATE s01;
066100060320         ENDDO;
066200060320
066300060320         sflNxtChg = *OFF;
066400060330
066500060330         IF prmImpTotale > 0 AND prmImpTotale <> prmImpImmesso;
066600060330           *IN99 = *ON;
066700060330           p01Importo = ReverseImage;
066800060330           errQuadratura = *ON;
066900060330           p01MsgId30 = 'Squadratura ' +
067000060330           %TRIML(%EDITC(prmImpImmesso-prmImpTotale:'K')) + '.';
067100060330         ENDIF;
067200060320
067300060320         IF NOT *IN99 AND dsp_aid = F6;
067400060321           opCode = 'UPDDB';
067500060320         ENDIF;
067600060320
067700060320       ENDSR;
067800060320
067900060320       //*********************************************************************************
068000060320       //
068100060428       // Lettura subfile assegni per scrittura database.
068200060320       //
068300060320       //*********************************************************************************
068400060321       BEGSR getC01UpdDb;
068500060320
068600060321         opCode = 'END';
068700060320         CLEAR esito;
068800060321
068900060321         DOU %EOF;
069000060321           READC s01;
069100060321           IF %EOF;
069200060321             LEAVE;
069300060321           ENDIF;
069400060328           EXSR wrtFidai00f;
069500060321           IF esito <> 0;
069600060321             LEAVE;
069700060321           ENDIF;
069800060321         ENDDO;
069900060323
070000060323         IF stampare;
070100060323           opCode = 'PRTDATA';
070200060323         ENDIF;
070300060320
070400060320       ENDSR;
070500060320
070600060320       //*********************************************************************************
070700060320       //
070800060329       // Scrittura FIDAI00F.
070900060320       //
071000060320       //*********************************************************************************
071100060328       BEGSR wrtFidai00f;
071200060320
071300060320         CLEAR esito;
071400060329      /END-FREE
071500060320
071600060329     c* controllo se esiste già l'assegno e nel caso lo cancello perchè
071700060329     c* è prevista l'insert dopo
071800060502     c     kdai          delete    fidai03l
071900060329      /FREE
072000060329         IF selv = '1';
072100060321      /END-FREE
072200060321     C/EXEC SQL
072300060328     C+ INSERT INTO QTEMP/FIDaI00F (DaIKNRAZ, DaIfle, DaItpd,
072400060328     C+ dailnp, daiaas, dainrs, dainsp, daicas, daivca, daicmb, dainra,
072500060328     C+ daidte, daiabi, daicab, daitpa, daitpi)
072600060428     C+ VALUES(:knraz, :prmfle, ' ', 0, 0, 0, 0, :cash,
072700111007     C+ :vcav, 0, :nrah, :dteh, :abiv, :cabv, :tpav, :prmtpiass)
072800060321     C/END-EXEC
072900060321      /FREE
073000060321           IF sqlCod < 0;
073100060321             opCode = 'ERRORE';
073200060321             prmRtnCode = 'ERRSQLINS';
073300060321             esito = sqlCod;
073400060321             LEAVESR;
073500060321           ENDIF;
073600060321         ENDIF;
073700060323
073800060320       ENDSR;
073900060320
074000060320       //*********************************************************************************
074100060320       //
074200060320       // Azzera totali.
074300060320       //
074400060320       //*********************************************************************************
074500060320       BEGSR azzeraTotali;
074600060320
074700060321         c01Importo = prmImpTotale;
074800060321         CLEAR prmImpImmesso;
074900120118         CLEAR prmnraImmesso;
075000060320
075100060320       ENDSR;
075200060320
075300060320       //*********************************************************************************
075400060320       //
075500060320       // Calcolo totali.
075600060320       //
075700060320       //*********************************************************************************
075800060320       BEGSR calcolaTotali;
075900060320
076000060330       if selv = '1';
076100060428         prmImpImmesso += cash;
076200120118         prmnraImmesso = prmnraImmesso + 1;
076300060330       endif;
076400060427       if selv = 'D';
076500111007         cnc0wb02r(prmfle: tpiv: tpav: dteh: nrah: abiv: cabv: vcav: cash
076600111007         :fileh);
076700111007         selv = ' ';
076800060427       endif;
076900060320
077000060320       ENDSR;
077100060322
077200060322       //*********************************************************************************
077300060322       //
077400060322       // Stampa dati immessi.
077500060322       //
077600060322       //*********************************************************************************
077700060323       BEGSR prtData;
077800060322
077900060322       ENDSR;
078000060321
078100060321       //*********************************************************************************
078200060321       //
078300060321       // Errore.
078400060321       //
078500060321       //*********************************************************************************
078600060321       BEGSR errore;
078700060321
078800060321         opCode = 'END';
078900060321         testo = 'Si è verificato l''errore ' + %TRIMR(prmRtnCode)
079000060321         + ' esito ' + %TRIML(%CHAR(esito))
079100060321         + '. Premere invio per continuare.';
079200060321         CLEAR qusec;
079300060321         qusbprv = %SIZE(qusec);
079400060321         DisplayLongText(testo:%SIZE(testo):'BAR0019':'YBARMSG   *LIBL'
079500060321         :qusec);
079600060321
079700060321       ENDSR;
079800060320
079900060320       //*********************************************************************************
080000060320       //
080100060320       // Uscita.
080200060320       //
080300060320       //*********************************************************************************
080400060320       BEGSR uscita;
080500060320
080600060321         IF prmRtnCode = ' ';
080700060321           prmRtnCode = 'DONE';
080800060321         ENDIF;
080900060328         kpjbu = cnc0wb00s;
081000060331         *inlr = *on;
081100060320
081200060320       ENDSR;
081300060320
081400060320      /END-FREE
