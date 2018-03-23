000100060321      //**********************************************************************************
000200060321      //
000300060427      // Questo programma visualizza il dettaglio degli assegni incassati.
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
001400060427     Fcnc0wb02d CF   E             WORKSTN
001500060320     F                                     INFDS(infDspF)
001600060320     F                                     INDDS(indDspF)
001700060320     F                                     SFILE(s01:s01Rrn)
001800060320
001900060321      //**********************************************************************************
002000060321      //
002100060321      // Definizione costanti.
002200060321      //
002300060321      //**********************************************************************************
002400060320     D F3              C                   X'33'
002500060320     D F5              C                   X'35'
002600060320     D F6              C                   X'36'
002700060320     D F8              C                   X'38'
002800060320     D F12             C                   X'3C'
002900060322     D F21             C                   X'B9'
003000060320     D Enter           C                   X'F1'
003100060320     D ReverseImage    C                   X'21'
003200060320     D HighIntensity   C                   X'22'
003300060320     D Blink           C                   X'28'
003400060321     D IgnoreDBCSdata...
003500060321     D                 C                   '0'
003600060321     D NeverPromptTheCommand...
003700060321     D                 C                   '0'
003800060321     D CommandRunning...
003900060321     D                 C                   '0'
004000060320
004100060321      //**********************************************************************************
004200060321      //
004300060321      // Definizione delle procedure esterne usate.
004400060321      //
004500060321      //**********************************************************************************
004600060427     D ProcessCommands...
004700060321     D                 PR
004800060320     D                                     EXTPGM('QCAPCMD')
004900060320     D  sourceCommandString...
005000060321     D                            32767A
005100060321     D                                     CONST
005200060321     D                                     OPTIONS(*VARSIZE)
005300060320     D  sourceCommandStringLength...
005400060320     D                               10I 0
005500060320     D                                     CONST
005600060320     D  optionsControlBlock...
005700060321     D                                     LIKE(QCAP0100)
005800060321     D                                     CONST
005900060321     D                                     OPTIONS(*VARSIZE)
006000060320     D  optionsControlBlockLength...
006100060320     D                               10I 0
006200060320     D                                     CONST
006300060320     D  optionsControlBlockFormat...
006400060320     D                                8A
006500060320     D                                     CONST
006600060320     D  changedCommandString...
006700060321     D                            32767A
006800060321     D                                     OPTIONS(*VARSIZE:*OMIT)
006900060320     D  lengthAvailableForChangedCommandString...
007000060320     D                               10I 0
007100060320     D                                     CONST
007200060320     D  lengthOfChangedCommandStringAvailableToReturn...
007300060320     D                               10I 0
007400060320     D  errorCode...
007500060321     D                                     LIKE(qusec)
007600060321     D                                     OPTIONS(*VARSIZE)
007700060321     D DisplayLongText...
007800060321     D                 PR
007900060321     D                                     EXTPGM('QUILNGTX')
008000060321     D  TextString...
008100060321     D                              256A
008200060321     D                                     CONST
008300060321     D                                     OPTIONS(*VARSIZE)
008400060321     D  LengthOfTextString...
008500060321     D                               10I 0
008600060321     D                                     CONST
008700060321     D  MessageID...
008800060321     D                                7A
008900060321     D                                     CONST
009000060321     D  QualifiedMessageFileName...
009100060321     D                               20A
009200060321     D                                     CONST
009300060321     D  ErrorCode...
009400060321     D                                     LIKE(qusec)
009500060321     D                                     OPTIONS(*VARSIZE)
009600060323
009700060321      //**********************************************************************************
009800060321      //
009900060321      // Definizione strutture dati.
010000060321      //
010100060321      //**********************************************************************************
010200060320     D infDspF         DS
010300060320     D  dsp_aid              369    369A                                        AID byte
010400060320     D  sf_rrn               376    377I 0                                      Subfile rr
010500060320     D indDspF         DS
010600060320     D  sflDspCtl             25     25N
010700060320     D  sflDsp                26     26N
010800060320     D  sflClr                27     27N
010900060320     D  sflEnd                28     28N
011000060321     D  sflNxtChg             29     29N
011100060321     D  errQuadratura         30     30N
011200060321     D  errGenerico           99     99N
011300060328     D cnassds       E DS                  extname(cnass00f)
011400060328     D                                     INZ
011500060322
011600060321      //**********************************************************************************
011700060321      //
011800060321      // Definizione variabili.
011900060321      //
012000060321      //**********************************************************************************
012100060322     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
012200060322     D/COPY QSYSINC/QRPGLESRC,QUSEC
012300060320     D nullPtr         S               *
012400060320     D rtnLib          S             10A
012500060320     D opCode          S             10A
012600060320     D                                     INZ('INZC01')
012700060320     D s01Rrn          S              5I 0
012800060320     D esito           S             10I 0
012900060320     D comando         S            256A
013000060321     D testo           S            256A
013100060323     D stampare        S              1N
013200060328     D dtaiso          S               d
013300060328     D dtaeur          S               d   datfmt(*eur)
013400060428     D £fle            S              3s 0
013500060428     D £tpa            S                   like(asstpa)
013600060428     D £tpi            S                   like(asstpi)
013700060427     D £dte            S                   like(assdte)
013800111007     D £nra            S                   like(assnra)
013900111007     D £file           S              1
014000060427     D £abi            S                   like(assabi)
014100060427     D £cab            S                   like(asscab)
014200060428     D £div            S                   like(assvca)
014300060428     D £cas            S                   like(asscas)
014400110810     D   tpd           S                   like(asstpd)
014500110810     D   cmb           S                   like(asscmb)
014600110810     D   dte           S                   like(assdte)
014700110810     D   tpa           S                   like(asstpa)
014800110810     D   aas           S                   like(assaas)
014900110810     D   lnp           S                   like(asslnp)
015000110810     D   nrs           S                   like(assnrs)
015100110810     D   nsp           S                   like(assnsp)
015200110810     D   nra           S                   like(assnra)
015300110810     D   tpi           S                   like(asstpi)
015400110810     D   abi           S                   like(assabi)
015500110810     D   cab           S                   like(asscab)
015600110810     D   vca           S                   like(assvca)
015700110810     D   cas           S                   like(asscas)
015800110810
015900110810      //**********************************************************************************
016000060322      //
016100060322      // Definizione aree dati.
016200060322      //
016300060322      //**********************************************************************************
016400060322     D §azute        E DS
016500060322     D                                     EXTNAME(azute00f)
016600060322     D                                     DTAARA
016700060322     D §DatiUte      E DS
016800060322     D                                     EXTNAME(dDatiUte)
016900060322     D                                     DTAARA
017000060328     D xsrda8          PR
017100060328     D                                     EXTPGM('XSRDA8')
017200060328     D  wlbda8
017300060328     D                                     LIKEDS(wlbda8)
017400060328     D wlbda8          DS
017500060328     D                                     INZ
017600060328     D  g08Dat                        8S 0
017700060328     D  g08Inv                        8S 0
017800060328     D  g08Err                        1A
017900060328     D  g08tgi                        5S 0
018000060322
018100060321      //**********************************************************************************
018200060321      //
018300060321      // Definizione parametri procedura.
018400060321      //
018500060321      //**********************************************************************************
018600060320     C     *ENTRY        PLIST
018700060428     C                   PARM                    £fle
018800060428     C                   PARM                    £tpi
018900060428     C                   PARM                    £tpa
019000060427     C                   PARM                    £dte
019100111007     C                   PARM                    £nra
019200060427     C                   PARM                    £abi
019300060427     C                   PARM                    £cab
019400060428     C                   PARM                    £div
019500060428     C                   PARM                    £cas
019600111007     C                   PARM                    £file
019700060320
019800060320      /FREE
019900060320
020000060320       DOU opCode = 'END';
020100060320         SELECT;
020200060320           WHEN opCode = 'END';
020300060320             LEAVE;
020400060320           WHEN opCode = 'INZC01';
020500060320             EXSR inzC01;
020600060320           WHEN opCode = 'PUTC01';
020700060320             EXSR putC01;
020800060321           WHEN opCode = 'ERRORE';
020900060321             EXSR errore;
021000060320           OTHER;
021100060320             LEAVE;
021200060320         ENDSL;
021300060320       ENDDO;
021400060320
021500060427         *inlr = *on;
021600060320
021700060320       //*********************************************************************************
021800060320       //
021900060320       // Operazioni iniziali da eseguire una tantum.
022000060320       //
022100060320       //*********************************************************************************
022200060320       BEGSR *INZSR;
022300060321      /END-FREE
022400060321     C/EXEC SQL
022500060321     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
022600060321     C/END-EXEC
022700060321      /FREE
022800060322       ENDSR;
022900060320
023000060320       //*********************************************************************************
023100060320       //
023200060321       // Inizializzo subfile contante.
023300060320       //
023400060320       //*********************************************************************************
023500060320       BEGSR inzC01;
023600060320
023700060321         opCode = 'PUTC01';
023800060320
023900060321         sflDspCtl = *OFF;
024000060320         sflDsp = *OFF;
024100060320         sflEnd = *OFF;
024200060320         sflClr = *ON;
024300060320         WRITE c01;
024400060321         sflDspCtl = *ON;
024500060320         sflDsp = *ON;
024600060320         sflClr = *OFF;
024700060320         sflNxtChg = *OFF;
024800060320         CLEAR s01Rrn;
024900111007       // se file = 'C' significa che ci sono + assegni x una
025000120118       // spedizione quindi devo leggere il TNCSM
025100111007        if £file ='C' ;
025200110812      /END-FREE
025300110812     C/EXEC SQL
025400110812     C+ DECLARE PIUASS CURSOR FOR SELECT asstpd, asscmb, assaas, asslnp,
025500120118     C+ assnrs, assnsp, assvca, (asstpa) tpa, (asstpi) tpi , csmdte dte,
025600120125     C+ (csmnra) nra, (csmabi) abi, (csmcab) cab, asscas FROM
025700120118     C+ cnass00F join tncsm00f on assnra = csmkey
025800120118     C+ WHERE assfle =:£fle and asstpi =
025900120118     C+ :£tpi and asstpa = :£tpa and csmdte=:£dte and csmnra=:£nra and
026000120118     C+ csmabi=:£abi and csmcab = :£cab
026100110812     C/END-EXEC
026200110812      /FREE
026300110812        else;
026400110812      /END-FREE
026500110812     C/EXEC SQL
026600110812     C+ DECLARE contante CURSOR FOR SELECT asstpd, asscmb, assaas, asslnp,
026700110812     C+ assnrs, assnsp, assvca, asstpa, asstpi , assdte,
026800110812     C+ assnra, assabi, asscab, asscas
026900110812     C+ from cnass00F where assfle =:£fle and asstpi = :£tpi and
027000111007     C+ asstpa = :£tpa and assdte=:£dte and assnra=:£nra
027100110812     C+ and assabi=:£abi and asscab = :£cab
027200110812     C/END-EXEC
027300110812      /FREE
027400110812        endif;
027500111007        if £file ='C';
027600110812      /END-FREE
027700110812     C/EXEC SQL
027800110812     C+ OPEN piuass
027900110812     C/END-EXEC
028000110812      /FREE
028100110812        else;
028200110812      /END-FREE
028300060328     C/EXEC SQL
028400060328     C+ OPEN CONTANTE
028500060321     C/END-EXEC
028600110812      /FREE
028700110812        endif;
028800060321
028900060321         IF sqlCod < 0;
029000060321           esito = sqlCod;
029100060321           opCode = 'ERRORE';
029200060321           LEAVESR;
029300060321         ENDIF;
029400060321
029500060321         DOU sqlCod = 100;
029600110812
029700111007        if £file ='C' ;
029800110812      /END-FREE
029900110812     C/EXEC SQL
030000110812     C+ FETCH piuass INTO
030100110812     C+ :tpd,
030200110812     C+ :cmb,
030300110812     C+ :aas,
030400110812     C+ :lnp,
030500110812     C+ :nrs,
030600110812     C+ :nsp,
030700110812     C+ :vca,
030800110812     C+ :tpa,
030900110812     C+ :tpi,
031000110812     C+ :dte,
031100110812     c+ :nra,
031200110812     C+ :abi,
031300110812     C+ :cab,
031400110812     C+ :cas
031500110812     C/END-EXEC
031600110812      /FREE
031700110812        else;
031800110812      /END-FREE
031900060321     C/EXEC SQL
032000110810     C+ FETCH CONTANTE INTO
032100110810     C+ :tpd,
032200110810     C+ :cmb,
032300110810     C+ :aas,
032400110810     C+ :lnp,
032500110810     C+ :nrs,
032600110810     C+ :nsp,
032700110810     C+ :vca,
032800110810     C+ :tpa,
032900110810     C+ :tpi,
033000110810     C+ :dte,
033100110810     c+ :nra,
033200110810     C+ :abi,
033300110810     C+ :cab,
033400110810     C+ :cas
033500060321     C/END-EXEC
033600110812      /FREE
033700110812        endif;
033800110812
033900060321           SELECT;
034000060321             WHEN sqlCod = 100;
034100110812             if s01Rrn = 0;
034200110812               opCode = 'END   ';
034300110812             endif;
034400060321               sflEnd = *ON;
034500060321               LEAVE;
034600060321             WHEN sqlCod < 0;
034700060321               esito = sqlCod;
034800060321               opCode = 'ERRORE';
034900060321               LEAVE;
035000060321             OTHER;
035100060321               EXSR setS01;
035200060328               sflNxtChg = (selv = '1');
035300060321               s01Rrn += 1;
035400060321               WRITE s01;
035500060321           ENDSL;
035600060321         ENDDO;
035700060321
035800111007        if £file ='C';
035900110812      /END-FREE
036000110812     C/EXEC SQL
036100110812     C+ CLOSE piuass
036200110812     C/END-EXEC
036300110812      /FREE
036400110812        else;
036500110812      /END-FREE
036600060330     C/EXEC SQL
036700060427     C+ CLOSE CONTANTE
036800060330     C/END-EXEC
036900110812      /FREE
037000110812       endif;
037100060320       ENDSR;
037200060321
037300060321       //*********************************************************************************
037400060321       //
037500060321       // Impostazione riga contante.
037600060321       //
037700060321       //*********************************************************************************
037800060321       BEGSR setS01;
037900060321
038000060328         selv = ' ';
038100110810         tpdh = tpd;
038200110810         cmbh = cmb;
038300110810         dteh = dte;
038400110810         tpav = tpa;
038500110810         nrah = nra;
038600110810         aasv = aas;
038700110810         lnpv = lnp;
038800110810         nrsv = nrs;
038900110810         nspv = nsp;
039000110810         nrav = nra;
039100110810         tpiv = tpi;
039200110810         abiv = abi;
039300110810         cabv = cab;
039400060328           RESET wlbda8;
039500110810           g08inv = dte;
039600060328           xsrda8(wlbda8);
039700060328           IF g08Err = '0';
039800060328             Dtev= g08dat;
039900060328           ENDIF;
040000110810         vcav = vca;
040100110810         casv = cas;
040200060321
040300060321       ENDSR;
040400060320
040500060320       //*********************************************************************************
040600060320       //
040700060321       // Emissione subfile contante.
040800060320       //
040900060320       //*********************************************************************************
041000060320       BEGSR putC01;
041100060320
041200060321         opCode = 'PUTC01';
041300060428
041400060428         c01Div = £div;
041500060428           c01Importo = £cas;
041600060321
041700060320         WRITE f01;
041800060320         EXFMT c01;
041900060320
042000060320         SELECT;
042100060320           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
042200060320             opCode = 'END';
042300060320           WHEN dsp_aid = F5;
042400060320             opCode = 'INZC01';
042500060322           WHEN dsp_aid = F21;
042600060323             stampare = *ON;
042700060320         ENDSL;
042800060320
042900060320       ENDSR;
043000060320
043100060321       //*********************************************************************************
043200060321       //
043300060321       // Errore.
043400060321       //
043500060321       //*********************************************************************************
043600060321       BEGSR errore;
043700060321
043800060321         opCode = 'END';
043900060427         testo = 'Si è verificato l''errore '
044000060321         + ' esito ' + %TRIML(%CHAR(esito))
044100060321         + '. Premere invio per continuare.';
044200060321         CLEAR qusec;
044300060321         qusbprv = %SIZE(qusec);
044400060321         DisplayLongText(testo:%SIZE(testo):'BAR0019':'YBARMSG   *LIBL'
044500060321         :qusec);
044600060321
044700060321       ENDSR;
044800060320
