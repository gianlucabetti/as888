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
004601060427     D ProcessCommands...
005900060321     D                 PR
006000060320     D                                     EXTPGM('QCAPCMD')
006100060320     D  sourceCommandString...
006200060321     D                            32767A
006300060321     D                                     CONST
006400060321     D                                     OPTIONS(*VARSIZE)
006500060320     D  sourceCommandStringLength...
006600060320     D                               10I 0
006700060320     D                                     CONST
006800060320     D  optionsControlBlock...
006900060321     D                                     LIKE(QCAP0100)
007000060321     D                                     CONST
007100060321     D                                     OPTIONS(*VARSIZE)
007200060320     D  optionsControlBlockLength...
007300060320     D                               10I 0
007400060320     D                                     CONST
007500060320     D  optionsControlBlockFormat...
007600060320     D                                8A
007700060320     D                                     CONST
007800060320     D  changedCommandString...
007900060321     D                            32767A
008000060321     D                                     OPTIONS(*VARSIZE:*OMIT)
008100060320     D  lengthAvailableForChangedCommandString...
008200060320     D                               10I 0
008300060320     D                                     CONST
008400060320     D  lengthOfChangedCommandStringAvailableToReturn...
008500060320     D                               10I 0
008600060320     D  errorCode...
008700060321     D                                     LIKE(qusec)
008800060321     D                                     OPTIONS(*VARSIZE)
008900060321     D DisplayLongText...
009000060321     D                 PR
009100060321     D                                     EXTPGM('QUILNGTX')
009200060321     D  TextString...
009300060321     D                              256A
009400060321     D                                     CONST
009500060321     D                                     OPTIONS(*VARSIZE)
009600060321     D  LengthOfTextString...
009700060321     D                               10I 0
009800060321     D                                     CONST
009900060321     D  MessageID...
010000060321     D                                7A
010100060321     D                                     CONST
010200060321     D  QualifiedMessageFileName...
010300060321     D                               20A
010400060321     D                                     CONST
010500060321     D  ErrorCode...
010600060321     D                                     LIKE(qusec)
010700060321     D                                     OPTIONS(*VARSIZE)
011200060323
011300060321      //**********************************************************************************
011400060321      //
011500060321      // Definizione strutture dati.
011600060321      //
011700060321      //**********************************************************************************
011800060320     D infDspF         DS
011900060320     D  dsp_aid              369    369A                                        AID byte
012000060320     D  sf_rrn               376    377I 0                                      Subfile rr
012100060320     D indDspF         DS
012200060320     D  sflDspCtl             25     25N
012300060320     D  sflDsp                26     26N
012400060320     D  sflClr                27     27N
012500060320     D  sflEnd                28     28N
012600060321     D  sflNxtChg             29     29N
012700060321     D  errQuadratura         30     30N
012800060321     D  errGenerico           99     99N
014001060328     D cnassds       E DS                  extname(cnass00f)
014002060328     D                                     INZ
014100060322
014200060321      //**********************************************************************************
014300060321      //
014400060321      // Definizione variabili.
014500060321      //
014600060321      //**********************************************************************************
014700060322     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
014800060322     D/COPY QSYSINC/QRPGLESRC,QUSEC
014900060320     D nullPtr         S               *
015000060320     D rtnLib          S             10A
015100060320     D opCode          S             10A
015200060320     D                                     INZ('INZC01')
015300060320     D s01Rrn          S              5I 0
015400060320     D esito           S             10I 0
015500060320     D comando         S            256A
015600060321     D testo           S            256A
015700060323     D stampare        S              1N
015701060328     D dtaiso          S               d
015702060328     D dtaeur          S               d   datfmt(*eur)
015703060428     D £fle            S              3s 0
015704060428     D £tpa            S                   like(asstpa)
015705060428     D £tpi            S                   like(asstpi)
015706060427     D £dte            S                   like(assdte)
015707060427     D £nra            S                   like(assnra)
015708060427     D £abi            S                   like(assabi)
015709060427     D £cab            S                   like(asscab)
015710060428     D £div            S                   like(assvca)
015711060428     D £cas            S                   like(asscas)
015800060321
015900060322      //**********************************************************************************
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
016901060328     D xsrda8          PR
016902060328     D                                     EXTPGM('XSRDA8')
016903060328     D  wlbda8
016904060328     D                                     LIKEDS(wlbda8)
016905060328     D wlbda8          DS
016906060328     D                                     INZ
016907060328     D  g08Dat                        8S 0
016908060328     D  g08Inv                        8S 0
016909060328     D  g08Err                        1A
016910060328     D  g08tgi                        5S 0
017000060322
017100060321      //**********************************************************************************
017200060321      //
017300060321      // Definizione parametri procedura.
017400060321      //
017500060321      //**********************************************************************************
017600060320     C     *ENTRY        PLIST
017700060428     C                   PARM                    £fle
017701060428     C                   PARM                    £tpi
017702060428     C                   PARM                    £tpa
017703060427     C                   PARM                    £dte
017704060427     C                   PARM                    £nra
017705060427     C                   PARM                    £abi
017706060427     C                   PARM                    £cab
017707060428     C                   PARM                    £div
017708060428     C                   PARM                    £cas
017800060320
017900060320      /FREE
018200060320
018300060320       DOU opCode = 'END';
018400060320         SELECT;
018500060320           WHEN opCode = 'END';
018600060320             LEAVE;
018700060320           WHEN opCode = 'INZC01';
018800060320             EXSR inzC01;
018900060320           WHEN opCode = 'PUTC01';
019000060320             EXSR putC01;
019700060321           WHEN opCode = 'ERRORE';
019800060321             EXSR errore;
019900060320           OTHER;
020100060320             LEAVE;
020200060320         ENDSL;
020300060320       ENDDO;
020400060320
020501060427         *inlr = *on;
020600060320
020700060320       //*********************************************************************************
020800060320       //
020900060320       // Operazioni iniziali da eseguire una tantum.
021000060320       //
021100060320       //*********************************************************************************
021200060320       BEGSR *INZSR;
021300060321      /END-FREE
021400060321     C/EXEC SQL
021500060321     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
021600060321     C/END-EXEC
021700060321      /FREE
022900060322       ENDSR;
023000060320
028900060320       //*********************************************************************************
029000060320       //
029100060321       // Inizializzo subfile contante.
029200060320       //
029300060320       //*********************************************************************************
029400060320       BEGSR inzC01;
029500060320
029600060321         opCode = 'PUTC01';
030100060320
030200060321         sflDspCtl = *OFF;
030300060320         sflDsp = *OFF;
030400060320         sflEnd = *OFF;
030500060320         sflClr = *ON;
030600060320         WRITE c01;
030700060321         sflDspCtl = *ON;
030800060320         sflDsp = *ON;
030900060320         sflClr = *OFF;
031000060320         sflNxtChg = *OFF;
031100060320         CLEAR s01Rrn;
031101060427      /END-FREE
031300060320
031500060321     C/EXEC SQL
031600060427     C+ DECLARE CONTANTE CURSOR FOR SELECT cnass00f.*
031700060428     C+ from cnass00F where assfle =:£fle and asstpi = :£tpi and
031701060427     C+ asstpa = :£tpa and assdte=:£dte and assnra=:£nra
031702060427     C+ and assabi=:£abi and asscab = :£cab
031906060328     C/END-EXEC
031907060328     C/EXEC SQL
031908060328     C+ OPEN CONTANTE
032400060321     C/END-EXEC
032401060328      /FREE
033000060321
033100060321         IF sqlCod < 0;
033300060321           esito = sqlCod;
033400060321           opCode = 'ERRORE';
033500060321           LEAVESR;
033600060321         ENDIF;
033700060321
033800060321         DOU sqlCod = 100;
033900060321      /END-FREE
034000060321     C/EXEC SQL
034100060427     C+ FETCH CONTANTE INTO :cnassds
034200060321     C/END-EXEC
034300060321      /FREE
034400060321           SELECT;
034500060321             WHEN sqlCod = 100;
034600060321               sflEnd = *ON;
034700060321               LEAVE;
034800060321             WHEN sqlCod < 0;
035000060321               esito = sqlCod;
035100060321               opCode = 'ERRORE';
035200060321               LEAVE;
035300060321             OTHER;
035400060321               EXSR setS01;
035500060328               sflNxtChg = (selv = '1');
035600060321               s01Rrn += 1;
035700060321               WRITE s01;
035900060321           ENDSL;
036000060321         ENDDO;
036100060321
036705060330      /END-FREE
036706060330     C/EXEC SQL
036707060427     C+ CLOSE CONTANTE
036708060330     C/END-EXEC
036709060427      /FREE
036800060320       ENDSR;
036900060321
037000060321       //*********************************************************************************
037100060321       //
037200060321       // Impostazione riga contante.
037300060321       //
037400060321       //*********************************************************************************
037500060321       BEGSR setS01;
037600060321
037604060328         selv = ' ';
037700060328         tpdh = asstpd;
037701060328         cmbh = asscmb;
037702060328         dteh = assdte;
037703060428         tpav = asstpa;
037704060329         nrah = assnra;
037705060328         aasv = assaas;
037706060328         lnpv = asslnp;
037707060328         nrsv = assnrs;
037708060328         nspv = assnsp;
037709060328         nrav = assnra;
037710060404         tpiv = asstpi;
037711060328         abiv = assabi;
037712060328         cabv = asscab;
037713060328           RESET wlbda8;
037714060328           g08inv = assdte;
037715060328           xsrda8(wlbda8);
037716060328           IF g08Err = '0';
037717060328             Dtev= g08dat;
037718060328           ENDIF;
037719060328         vcav = assvca;
037720060328         casv = asscas;
037800060321
037900060321       ENDSR;
038000060320
038100060320       //*********************************************************************************
038200060320       //
038300060321       // Emissione subfile contante.
038400060320       //
038500060320       //*********************************************************************************
038600060320       BEGSR putC01;
038700060320
038800060321         opCode = 'PUTC01';
038801060428
038802060428         c01Div = £div;
038804060428           c01Importo = £cas;
039600060321
039700060320         WRITE f01;
039800060320         EXFMT c01;
040100060320
040200060320         SELECT;
040300060320           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
040400060320             opCode = 'END';
040500060320           WHEN dsp_aid = F5;
040600060320             opCode = 'INZC01';
040900060322           WHEN dsp_aid = F21;
041000060323             stampare = *ON;
041100060320         ENDSL;
041200060320
041300060320       ENDSR;
041400060320
056000060321       //*********************************************************************************
056100060321       //
056200060321       // Errore.
056300060321       //
056400060321       //*********************************************************************************
056500060321       BEGSR errore;
056600060321
056700060321         opCode = 'END';
056800060427         testo = 'Si è verificato l''errore '
056900060321         + ' esito ' + %TRIML(%CHAR(esito))
057000060321         + '. Premere invio per continuare.';
057100060321         CLEAR qusec;
057200060321         qusbprv = %SIZE(qusec);
057300060321         DisplayLongText(testo:%SIZE(testo):'BAR0019':'YBARMSG   *LIBL'
057400060321         :qusec);
057500060321
057600060321       ENDSR;
057700060320
