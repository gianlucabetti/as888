000100061215     H*--------------------------------------------------------------------------------------------*
000200070202     H* Verifica abbinamenti distinte/giri
000300061215     H*--------------------------------------------------------------------------------------------*
000400061215     H DECEDIT('0,') DATEDIT(*DMY.)
000500061215     F*--------------------------------------------------------------------------------------------*
000600061215     F* Data base
000700061215     F*--------------------------------------------------------------------------------------------*
000800070202     FFNARB01L  IF   E           K DISK
000900070202     FTABEL00F  IF   E           K DISK
001000070202     FTNTBE01L  IF   E           K DISK
001100061215     FTIST94P   O    E             PRINTER OFLIND(*IN05) USROPN
001200061215     D*--------------------------------------------------------------------------------------------*
001300061215     D* Data structure
001400061215     D*--------------------------------------------------------------------------------------------*
001500061215     D*------------------
001600061215     D* DS ARCHITETTURA
001700061215     D*------------------
001800061215     D KPJBA         E DS
001900061215     D*-------------------
002000061215     D* RICERCA PARAMETRI UTENTI
002100061215     D*-------------------
002200061215     D TIBS34DS      E DS                                                       *Profili utente
002300061215     D DDATIUTE      E DS                                                       *Dati utente
002400061215     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002500070202     D*-------------------
002600070202     D* DS PER TRUL06R - CARICAMENTO £1/£6
002700070202     D*-------------------
002800070202     D TRUL06DS      E DS
002900070202     D  Lin                    1     90  0
003000070202     D                                     DIM(30)                              SKI COMODO
003100070125     D*-------------------
003200070125     D* DS PROCEDURA CONTROLLO/REPERIMENTO GIRI
003300070125     D*-------------------
003400070125     D FIDG09DS      E DS
003401120403     D dgrgflo       E DS
003402120403     D dlimmicro     E DS
003500061215     D*------------------
003600061215     D* DS PASSAGGIO PARAMETRI
003700061215     D*------------------
003800061215     D PARAM           DS
003900061215     D  In_FGS                        3  0
004000061215     D  In_DFV                        8  0
004100061215     D  In_NFV                        7  0
004200070201     D  In_LIV                        1
004300061220     D  In_TIP                        2
004400070223     D  In_PIG                        1
004500061215     D*------------------
004600061215     D* DS DI WRK
004700061215     D*------------------
004800061215     D DS_WST94_DET    DS
004900061215     D  DET_FGS                       3  0
005000061215     D  DET_NPG                       1
005100061215     D  DET_NFV                       7  0
005200061215     D  DET_DFV                       8  0
005300061215     D  DET_FPP                       1
005400061215     D  DET_PDR                       7  0
005500061215     D  DET_TIP                       1
005600061215     D  DET_RSC                      35
005601120403     D  DET_FLR                      30
005700061215     D  DET_CGI                      10
005800070215     D  DET_TGI                       1
005900070129     D  DET_PTY                       2  0
006000061222     D  DET_LNO                       3
006100061222     D  DET_ERR                       2
006200061215     D  DET_AAS                       4  0
006300061215     D  DET_LNP                       3  0
006400061215     D  DET_NRS                       2  0
006500061215     D  DET_NSP                       7  0
006600070202     D  DET_LNA                       3  0
006700061215     D  DET_NCL                       5  0
006800061221     D  DET_PES                       7  1
006900061221     D  DET_VOL                       5  3
007000061220     D  DET_RSD                      35
007100061220     D  DET_IND                      35
007200061220     D  DET_LOD                      35
007300061220     D  DET_CAD                      35
007400061221     D  DET_PRD                       2
007500061220     D  DET_ZNC                       2  0
007501120404     D  DET_MCP        s              6
007600061215     D*
007700061215     D DS_WST94_TOTD   DS
007800061215     D  TOTD_NFV                      7  0
007900061218     D  TOTD_BOL                      9  0
008000061219     D  TOTD_NCL                      9  0
008100061221     D  TOTD_PES                      9  0
008200061221     D  TOTD_VOL                      9  2
008300061215     D*
008400061215     D DS_WST94_TOTG   DS
008500061215     D  TOTG_NFV                      7  0
008600061215     D  TOTG_CGI                     10
008700061215     D  TOTG_BOL                      9  0
008800061219     D  TOTG_NCL                      9  0
008900061221     D  TOTG_PES                      9  0
009000061221     D  TOTG_VOL                      9  2
009001120403     D*
009002120403     D DS_WST94_TGM    DS
009003120403     D  TGm_NFV                       7  0
009004120403     D  TGm_CGI                      10
009005120403     D  TGm_BOL                       9  0
009006120403     D  TGm_NCL                       9  0
009007120403     D  TGm_PES                       9  0
009008120403     D  TGm_VOL                       9  2
009100061215     D*------------------
009200061215     D* DS "XSRDA8" - CONTROLLA DATA (8)
009300061215     D*------------------
009400061215     D WLBDA8          DS
009500061215     D  G08DAT                 1      8  0
009600061215     D  G08INV                 9     16  0
009700061215     D  G08ERR                17     17
009800061215     D  G08TGI                18     22  0
009900070202     D*------------------
010000070202     D* SCHIERE D WRK
010100070202     D*------------------
010200070202     D cchco           S              2    DIM(30)
010300070202     D REC             S              2    DIM(50)
010400070202     D*------------------
010500070202     D* DS TABELLE
010600070202     D*------------------
010700070202     D DCCH          E DS
010800070202     D DS3A          E DS
010900070202     D*------------------
011000070202     D* DS controllo della presenza di CA per la spedizione richiesta
011100070202     D*------------------
011200070202     D FIDN12DS      e ds
011300070202     D  skKey                 26    305    dim(20)
011400070202     D  skAnn                306    325    dim(20)
011500070202     D  skDca                326    485  0 dim(20)
011600070202     D  skFca                486    545  0 dim(20)
011700070202     D  skDch                546    705  0 dim(20)
011800070202     D  skCch                706    745    dim(20)
011900070202     D*
012000070202     D dsKey           ds
012100070202     D  dsaac                         4  0
012200070202     D  dsfil                         3  0
012300070202     D  dsnca                         7  0
012400061215     D*------------------
012500061215     D* VARIABILI D WRK
012600061215     D*------------------
012700061215     D FGS_DA          S              3  0 inz(*loval)
012800061215     D FGS_A           S              3  0 inz(*hival)
012900061215     D DFV_DA          S              8  0 inz(*loval)
013000061215     D DFV_A           S              8  0 inz(*hival)
013100061215     D NFV_DA          S              7  0 inz(*loval)
013200061215     D NFV_A           S              7  0 inz(*hival)
013300061222     D SAV_NFV         S                   like(DET_NFV) inz
013400061222     D SAV_CGI         S                   like(DET_CGI) inz
013500070215     D SAV_PTY         S                   like(DET_PTY) inz
013600061222     D In_DFV_stp      S                   like(In_DFV)  inz
013700070226     D In_PIG_num      S              1  0 inz
013800070117     D wTotBolle       S              7  0 inz(*zeros)
013801120403     D wTotBollem      S              7  0 inz(*zeros)
013900070202     D wTBEcod         S                   like(tbecod)
014000070202     D KTBkut          S                   like(tblkut) inz(1)
014100070202     D KTBcod          S                   like(tblcod)
014200061215     C*--------------------------------------------------------------------------------------------*
014300061215     C* Main lines
014400061215     C*--------------------------------------------------------------------------------------------*
014500071019     C*
014600071019     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
014700071019     C
014800071019     C/EXEC SQL
014900071019     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
015000071019     C/END-EXEC
015100061215     C*
015200061215     C* Reperimento dati utente
015300061215     C                   exsr      repdatiute
015400070202     C*
015500070202     C* Caricamento P.O. in £6
015600070202     C                   exsr      carlira
015700070202     C*
015800070202     C* Caricamento dati tabellati
015900070202     C                   exsr      cartab
016000061215     C*
016100061215     C* Verifica parametri d lancio
016200061215     C                   exsr      chkpar
016300061215     C*
016400061215     C* Deleto la tabella temporanea d wrk
016500061215     C                   exsr      dlttab
016600061215     C*
016700061215     C* Creo la tabella temporanea d wrk
016800061215     C                   exsr      crttab
016900061215     C*
017000061214     C* Reperisco i dati e popolo la tabella temporanea
017100070205     C                   if        In_DFV   = *zeros AND
017200070205     C                             In_NFV   = *zeros
017300070205     C                   exsr      repdatinodfv
017400070205     C                   else
017500070205     C                   exsr      repdatisidfv
017600070205     C                   endif
017700070202     C*
017800070202     C* Se lancio x bolle nn in distinta => eseguo scrematura bolle
017900070202     C                   exsr      exclboll
018000061214     C*
018100061220     C* A seconda del tipo lancio eseguo:
018200061214     C* - distinta/giro
018300070215     C                   if        In_TIP  = 'DG' and
018400070215     C                             In_LIV <> 'T'
018500061214     C                   exsr      sta_ndc_cgi
018600061220     C                   endif
018700070215     C* - distinta/priorità
018800070215     C                   if        In_TIP = 'DG' and
018900070215     C                             In_LIV = 'T'
019000070215     C                   exsr      sta_ndc_pty
019100061220     C                   endif
019200061220     C* - giro
019300061220     C                   if        In_TIP = 'G'
019400061220     C                   seton                                        52
019500061220     C                   exsr      sta_cgi
019600061220     C                   endif
019700061215     C*
019800061215     C* Termine programma
019900061215     C                   seton                                        lr
020000060515     C*
020100061215     C*------------------------------------------------------------------------*
020200061215     C* CHKPAR - Verifica parametri d lancio
020300061215     C*------------------------------------------------------------------------*
020400061215     C     CHKPAR        BEGSR
020500061215     C*
020600061215     C* P.O. gestione
020700070202     C****               eval      FGS_DA = In_FGS
020800070202     C****               eval      FGS_A  = In_FGS
020900061215     C*
021000061215     C* Data distinta
021100061215     C                   if        In_DFV <> *zeros
021200061215     C                   eval      DFV_DA = In_DFV
021300061215     C                   eval      DFV_A  = In_DFV
021400061222     C* - giro la data distinta
021500061222     C                   z-add     In_DFV        G08INV
021600061222     C                   z-add     *ZEROS        G08DAT
021700061222     C                   movel     '3'           G08ERR
021800061222     C                   call      'XSRDA8'
021900061222     C                   parm                    WLBDA8
022000061222     C                   z-add     G08DAT        In_DFV_stp
022100061215     C                   endif
022200061215     C*
022300061215     C* Numero distinta
022400061215     C                   if        In_NFV <> *zeros
022500061215     C                   eval      NFV_DA = In_NFV
022600061215     C                   eval      NFV_A  = In_NFV
022700061215     C                   endif
022800070202     C*
022900070202     C* Se lancio x bolle NN in distinta verifico consegnabilità d ogni bolla
023000070202     C                   if        In_DFV   = *zeros AND
023100070202     C                             In_NFV   = *zeros
023200070202     C                   eval      DFV_DA = *loval
023300070202     C                   eval      DFV_A  = *hival
023400070205     C                   eval      NFV_DA = *zeros
023500070205     C                   eval      NFV_A  = *zeros
023600070202     C                   endif
023700061215     C*
023800061215     C                   ENDSR
023900061215     C*------------------------------------------------------------------------*
024000061215     C* DLTTAB - Routine di cancellazione tabella temporanea
024100061215     C*------------------------------------------------------------------------*
024200061215     C     DLTTAB        BEGSR
024300061215     C*
024400061215     C/EXEC SQL
024500061215     C+ DROP TABLE QTEMP/WFST940F
024600061215     C/END-EXEC
024700061215     C*
024800061215     C                   ENDSR
024900061214     C*------------------------------------------------------------------------*
025000061214     C* CRTTAB - Routine di creazione tabella temporanea
025100061214     C*------------------------------------------------------------------------*
025200061214     C     CRTTAB        BEGSR
025300061214     C*
025400061214     C/EXEC SQL
025500061214     C+ CREATE TABLE QTEMP/WFST940F (
025600061214     C+ WST94FGS DEC (3, 0) NOT NULL WITH DEFAULT,
025700061214     C+ WST94NPG CHAR (1 ) NOT NULL WITH DEFAULT,
025800061214     C+ WST94NFV DEC (7, 0) NOT NULL WITH DEFAULT,
025900061214     C+ WST94DFV DEC (8, 0) NOT NULL WITH DEFAULT,
026000061214     C+ WST94FPP CHAR (1 ) NOT NULL WITH DEFAULT,
026100061214     C+ WST94PDR DEC (7, 0) NOT NULL WITH DEFAULT,
026200061214     C+ WST94TIP CHAR (1 ) NOT NULL WITH DEFAULT,
026300061214     C+ WST94RSC CHAR (35 ) NOT NULL WITH DEFAULT,
026301120404     C+ WST94FLR char (30 ) NOT NULL WITH DEFAULT,
026400061214     C+ WST94CGI CHAR (10 ) NOT NULL WITH DEFAULT,
026500070215     C+ WST94TGI CHAR (1 ) NOT NULL WITH DEFAULT,
026600070129     C+ WST94PTY DEC (2, 0) NOT NULL WITH DEFAULT,
026700061222     C+ WST94LNO CHAR (3 ) NOT NULL WITH DEFAULT,
026800061222     C+ WST94ERR CHAR (2 ) NOT NULL WITH DEFAULT,
026900061214     C+ WST94AAS DEC (4, 0) NOT NULL WITH DEFAULT,
027000061214     C+ WST94LNP DEC (3, 0) NOT NULL WITH DEFAULT,
027100061214     C+ WST94NRS DEC (2, 0) NOT NULL WITH DEFAULT,
027200061214     C+ WST94NSP DEC (7, 0) NOT NULL WITH DEFAULT,
027300070202     C+ WST94LNA DEC (3, 0) NOT NULL WITH DEFAULT,
027400061214     C+ WST94NCL DEC (5, 0) NOT NULL WITH DEFAULT,
027500061221     C+ WST94PES DEC (7, 1) NOT NULL WITH DEFAULT,
027600061221     C+ WST94VOL DEC (5, 3) NOT NULL WITH DEFAULT,
027700061220     C+ WST94RSD CHAR (35 ) NOT NULL WITH DEFAULT,
027800061220     C+ WST94IND CHAR (35 ) NOT NULL WITH DEFAULT,
027900061220     C+ WST94LOD CHAR (35 ) NOT NULL WITH DEFAULT,
028000061220     C+ WST94CAD CHAR (9 ) NOT NULL WITH DEFAULT,
028100061221     C+ WST94PRD CHAR (2 ) NOT NULL WITH DEFAULT,
028200120403     C+ WST94ZNC DEC (2, 0) NOT NULL WITH DEFAULT)
028300061214     C/END-EXEC
028400061214     C*
028500061214     C                   ENDSR
028600061214     C*------------------------------------------------------------------------*
028700070205     C* REPDATISIDFV - Routine di reperimento dati rispetto alle selezioni a video - si distinta
028800061214     C*------------------------------------------------------------------------*
028900070202     C     REPDATISIDFV  BEGSR
029000061214     C*
029100061214     C/EXEC SQL
029200070129     C+ insert into qtemp/wfst940f
029300061220     C+ select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr,
029400120403     C+ apdtip, apdrsc, apdflr, argcgi, argtgi,
029500070223     C+ getcgipty(apdtip, dstpdr, fvvfpp, fvvfgs, argcgi, fvvdfv,
029600070226     C+ fvvnfv, :In_PIG_num),
029700070129     C+ arglno, argerr,
029800070202     C+ arbaas, arblnp, arbnrs, arbnsp, arblna,
029900061221     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
030000061221     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
030100120403     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
030200061214     C+ from fiarg00f join
030300061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr, apdtip,
030400120403     C+ apdrsc, apdflr,
030401120403     C+ arbaas, arblnp, arbnrs, arbnsp, arblna,
030500061221     C+ arbncl, arbncr, arbncp, arbpkf, arbpkc, arbfvf, arbvlf, arbvlc,
030600061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
030700061214     C+ from fnarb00f join
030800061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp,
030900120403     C+ dstpdr, apdtip, apdrsc, apdflr
030901120403     C+ from fnfvv00f join
031000120403     C+ (select dstfgs, dstnfv, dstpdr, apdtip, apdrsc,
031100120403     C+ apdflr from fndst00f join fiapd00f on dstpdr=apdpdr
031200070206     C+ where apdtip='A') as padroncini
031300061214     C+ on fvvfgs=dstfgs and fvvnfv=dstnfv where fvvnpg='4' and fvvfcf=' '
031400061214     C+ and fvvdfv >= :DFV_DA  and fvvdfv <= :DFV_A
031500061214     C+ and fvvnfv >= :NFV_DA  and fvvnfv <= :NFV_A) as distinte on
031600061214     C+ fvvfgs=arbifp and fvvnfv=arbndc and fvvdfv=arbddc) as bolle on
031700061214     C+ arbaas=argaas and arblnp=arglnp and
031800061214     C+ arbnrs=argnrs and arbnsp=argnsp
031900061214     C+ union
032000061220     C+ select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr,
032100120403     C+ apdtip, apdrsc, apdflr, argcgi, argtgi,
032200070223     C+ getcgipty(apdtip, dstpdr, fvvfpp, fvvfgs, argcgi, fvvdfv,
032300070226     C+ fvvnfv, :In_PIG_num),
032400070129     C+ arglno, argerr,
032500070202     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp, arblna,
032600061221     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
032700061221     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
032800061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
032900061221     C+ from fnarb00f join
033000061220     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr,
033100120403     C+ apdtip, apdrsc, apdflr,
033101120403     C+ argcgi, argtgi, arglno, argerr,
033200070202     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp,
033300061214     C+ ftdncl, ftdpkl, ftdvlu
033400061214     C+ from fiarg00f join
033500061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr, apdtip,
033600120403     C+ apdrsc, apdflr,
033601120403     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp, ftdncl, ftdpkl, ftdvlu
033700061214     C+ from fiftd00f join
033800061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp,
033900120403     C+ dstpdr, apdtip, apdrsc, apdflr
033901120403     C+ from fnfvv00f join
034000120403     C+ (select dstfgs, dstnfv, dstpdr, apdtip, apdrsc, apdflr
034100120403     C+  from fndst00f join fiapd00f on dstpdr=apdpdr
034200070206     C+ where apdtip='A') as padroncini on
034300061214     C+ fvvfgs=dstfgs and fvvnfv=dstnfv where fvvnpg='4' and fvvfcf<>' '
034400061214     C+ and fvvdfv >= :DFV_DA  and fvvdfv <= :DFV_A
034500061214     C+ and fvvnfv >= :NFV_DA  and fvvnfv <= :NFV_A) as distinte on
034600070208     C+ fvvfgs=ftdfgs and fvvdfv=ftdddc and fvvnfv=ftdndc
034700061214     C+ where ftdtsr='C') as bolle on
034800061214     C+ ftdaas=argaas and ftdlnp=arglnp and
034900061220     C+ ftdnrs=argnrs and ftdnsp=argnsp) as estrazione on
035000061220     C+ ftdaas=arbaas and ftdlnp=arblnp and
035100061220     C+ ftdnrs=arbnrs and ftdnsp=arbnsp
035200061214     C/END-EXEC
035300061214     C*
035400061214     C                   ENDSR
035500070202     C*------------------------------------------------------------------------*
035600070205     C* REPDATINODFV - Routine di reperimento dati rispetto alle selezioni a video - no distinta
035700070202     C*------------------------------------------------------------------------*
035800070202     C     REPDATINODFV  BEGSR
035900070202     C*
036000070205     C/EXEC SQL
036100070205     C+ insert into qtemp/wfst940f
036200070608     C*+ select 0, ' ', 0, 0, ' ', 0, ' ', ' ', argcgi, argtgi, 0,
036300070608     C+ select :In_FGS, ' ', 0, 0, ' ', 0, ' ', ' ', argcgi, argtgi, 0,
036400070205     C+ arglno, argerr, arbaas, arblnp, arbnrs, arbnsp, arblna,
036500070205     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
036600070205     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
036700070205     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
036800070205     C+ from filtra201/fnarb00f join filtra201/fiarg00f
036900070205     C+ on arbaas=argaas and arblnp=arglnp and arbnrs=argnrs and
037000070205     C+ arbnsp=argnsp
037100070205     C+ where arbndc=0
037200070205     C/END-EXEC
037300070202     C*
037400070202     C                   ENDSR
037500070202     C*------------------------------------------------------------------------*
037600070202     C* EXCLBOLL - Routine di esclusione bolle
037700070202     C*------------------------------------------------------------------------*
037800070202     C     EXCLBOLL      BEGSR
037900070205     C*
038000070202     C/EXEC SQL
038100070202     C+ declare C9 cursor for
038200070202     C+ select * from wfst940f
038300070202     C/END-EXEC
038400070202     C
038500070202     C/EXEC SQL
038600070202     C+ open C9
038700070202     C/END-EXEC
038800070202     C
038900070202     C/EXEC SQL
039000070202     C+ Fetch next from C9 into :DS_WST94_DET
039100070202     C/END-EXEC
039200070202     C
039300070202     C*
039400070202     C* Se reperito qualcosa => analizzo
039500070202     C                   dow       sqlcod = *zeros
039600070205     C*
039700070205     C                   movel     'N'           Escbolla          1
039800070202     C*
039900070202     C* Escludo bolle nn in "lira" del P.O. richiesto
040000070202     C     DET_LNA       lookup    Lin                                    99
040100070202     C                   if        not *IN99 = *on
040200070202     C                   eval      Escbolla='S'
040300070202     C                   endif
040400070202     C*
040500070202     C* Se lancio x bolle NN in distinta verifico consegnabilità d ogni bolla
040600070202     C                   if        In_DFV   = *zeros AND
040700070202     C                             In_NFV   = *zeros AND
040800070202     C                             Escbolla = 'N'
040900070202     C*
041000070202     C* Aggancio la bolla
041100070202     C     KEYarb01_c    chain     fnarb01l
041200070202     C                   if        %found(fnarb01l)
041300070202     C*
041400070202     C* Verifico consegnabilità bolla
041500070202     C                   exsr      escludi
041600070202     C*
041700070202     C                   endif
041800070202     C                   endif
041900070202     C*
042000070202     C* Se bolla da escludere => elimino il record corrente dalla tabella sql
042100070202     C                   if        Escbolla = 'S'
042200070202     C/EXEC SQL
042300070202     C+ delete from wfst940f where CURRENT OF C9
042400070202     C/END-EXEC
042500070202     C                   endif
042600070202     C*
042700070202     C* Proseguo con la lettura della tabella sql
042800070202     C/EXEC SQL
042900070202     C+ Fetch next from C9 into :DS_WST94_DET
043000070202     C/END-EXEC
043100070202     C*
043200070202     C                   enddo
043300070202     C*
043400070202     C/EXEC SQL
043500070202     C+ close C9
043600070202     C/END-EXEC
043700070202     C*
043800070202     C                   ENDSR
043900060516     C*------------------------------------------------------------------------*
044000061214     C* STA_NDC_CGI - Routine di stampa x distita/giro
044100060516     C*------------------------------------------------------------------------*
044200061214     C     STA_NDC_CGI   BEGSR
044300061215     C*
044400061215     C* Apertura printer-file
044500061215     C                   open      TIST94P
044600061220     C*
044700061220     C* Valorizzo titolo tabulato
044800070125     C                   eval      stttes1 = '   *** VERIFICA ABBINAMENTI ' +
044900070117     C                                       'DISTINTE/GIRI ***'
045000070201     C                   if        In_LIV = 'S'
045100070125     C                   eval      stttes2 = '(SINTETICA)'
045200070117     C                   else
045300070201     C                   eval      stttes2 = '(DETTAGLIO)'
045400070201     C                   endif
045500061222     C*
045600061222     C* Valorizzo i paramteri d lancio
045700120404     C                   eval      stppar = 'Fil.: ' +
045800070201     C                                       %trim(%editw(In_FGS:'0   '))
045900061222     C                   if        In_NFV <> *zeros
046000070116     C                   eval      stppar = %trim(stppar) + ' '+
046100070116     C                                      'PARAMETRI: tutti le bolle nella ' +
046200061222     C                                      'distinta '                        +
046300061222     C                                       %trim(%editw(In_FGS:'0   '))      +
046400061222     C                                       ' '                               +
046500061222     C                                       %trim(%editc(In_NFV:'4'))
046600061222     C                   else
046700070201     C                   if        In_DFV <> *zeros
046800070116     C                   eval      stppar = %trim(stppar) + ' '+
046900070116     C                                      'PARAMETRI: tutti le bolle nelle ' +
047000061222     C                                      'distinte in data '                +
047100061222     C                                      %editw(In_DFV_stp:'0  /  /    ')
047200070201     C                   else
047300070201     C                   eval      stppar = %trim(stppar) + ' '+
047400070201     C                                      'PARAMETRI: tutti le bolle non ' +
047500070201     C                                      'inserite in nessuna distinta'
047600070201     C                   endif
047700061222     C                   endif
047800061218     C*
047900061218     C* Se scelta stampa nn dettaglio stampo subito la testata
048000061220     C                   if        *in40 = *off
048100061218     C                   write     st94te
048200061222     C                   seton                                        60
048300061220     C                   write     st94dte
048400061220     C                   write     st94ri
048500061222     C                   setoff                                       05
048600061218     C                   endif
048700060516     C*
048800060516     C/EXEC SQL
048900061215     C+ declare C1 cursor for
049000061214     C+ select * from wfst940f
049100070205     C+ order by wst94nfv, wst94pty, wst94cgi,
049200070205     C+ wst94prd, wst94lod, wst94ind, wst94cad, wst94rsd
049300061214     C+ for read only
049400060516     C/END-EXEC
049500060516     C
049600060516     C/EXEC SQL
049700061215     C+ open C1
049800060516     C/END-EXEC
049900060516     C
050000060516     C/EXEC SQL
050100061215     C+ Fetch next from C1 into :DS_WST94_DET
050200060516     C/END-EXEC
050300060516     C*
050400060516     C* Se reperito qualcosa => analizzo
050500061214     C                   dow       sqlcod = *zeros
050600061220     C*
050700061220     C* Valorizzo i dati in stampa relativi alla distinta
050800061220     C*
050900061220     C* - codice distinta
051000061220     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
051100061220     C                                       ' ' +
051200061220     C                                       %trim(%editc(det_nfv:'4'))
051300061220     C* - autotrasportatore
051400061220     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
051500061220     C                                       ' ' +
051600061220     C                                       det_rsc
051601120403     c                   if        %subst(det_flr: 9: 7) <> ' '
051602120404     c                   eval      det_mcp = %subst(det_flr: 9: 6)
051603120403     c                   move      det_mcp       stpmcp
051604120403     c                   else
051605120403     c                   clear                   stpmcp
051606120403     c                   end
051700061220     C* - data distinta
051800061220     C                   z-add     det_dfv       G08INV
051900061220     C                   z-add     *ZEROS        G08DAT
052000061220     C                   movel     '3'           G08ERR
052100061220     C                   call      'XSRDA8'
052200061220     C                   parm                    WLBDA8
052300061220     C                   z-add     G08DAT        stpdat
052400061220     C* - prestazione distinta
052500061220     C                   if        det_fpp = 'M'
052600061220     C                   eval      stpprest = 'MAT.'
052700061220     C                   endif
052800061220     C                   if        det_fpp = 'P'
052900061220     C                   eval      stpprest = 'POM.'
053000061220     C                   endif
053100061218     C*
053200061218     C* Se scelta stampa anche dettaglio ad ogni distinta salto pagina
053300061220     C                   if        *in40 = *on
053400061220     C                   eval      stpdes = 'Distinta: '+
053500061220     C                                %trim(stpdist)+' del '+
053600061220     C                                %editw(stpdat:'0  /  /    ')+' '+stpprest+
053700061220     C                                '    '+'Autotrasportatore: '+
053800061220     C                                stpaut
053900061222     C   60              write     st94ri
054000061222     C   60              write     st94er
054100061220     C                   write     st94te
054200061222     C                   seton                                        60
054300061222     C                   setoff                                       05
054400061218     C                   endif
054500061218     C*
054600061220     C* Inizializzo l'indicatore d nuova distinta
054700061218     C                   setoff                                       51
054800061215     C*
054900061215     C* Salvo la distinta corrente
055000061215     C                   eval      SAV_NFV = DET_NFV
055100061215     C*
055200061215     C/EXEC SQL
055300061215     C+ declare C2 cursor for
055400061219     C+ select wst94nfv, count(*), decimal(sum(wst94ncl), 9, 0),
055500061221     C+ decimal(sum(wst94pes), 9, 0), decimal(sum(wst94vol), 9, 2)
055600061219     C+ from wfst940f
055700061219     C+ where wst94nfv = :SAV_NFV
055800061221     C+ group by wst94nfv
055900061215     C+ for read only
056000061215     C/END-EXEC
056100061215     C
056200061215     C/EXEC SQL
056300061215     C+ open C2
056400061215     C/END-EXEC
056500061215     C
056600061215     C/EXEC SQL
056700061218     C+ Fetch next from C2 into :DS_WST94_TOTD
056800061215     C/END-EXEC
056900061215     C
057000061215     C/EXEC SQL
057100061215     C+ close C2
057200061215     C/END-EXEC
057300061215     C*
057400061215     C* Ad ogni rottura d distinta reperisco il totale bolle in distinta
057500061215     C                   dow       sqlcod = *zeros AND
057600061215     C                             DET_NFV = SAV_NFV
057700061215     C*
057800061215     C* Salvo il giro corrente
057900061215     C                   eval      SAV_CGI = DET_CGI
058000061215     C*
058100061215     C/EXEC SQL
058200061215     C+ declare C3 cursor for
058300070129     C+ select wst94nfv, wst94cgi, count(*),
058400070129     C+ decimal(sum(wst94ncl), 9, 0),
058500061221     C+ decimal(sum(wst94pes), 9, 0),
058600061221     C+ decimal(sum(wst94vol)+0.005, 9, 2)
058700061219     C+ from wfst940f
058800061215     C+ where wst94nfv = :SAV_NFV and wst94cgi = :SAV_CGI
058900061221     C+ group by wst94nfv, wst94cgi
059000061215     C+ for read only
059100061215     C/END-EXEC
059200061215     C
059300061215     C/EXEC SQL
059400061215     C+ open C3
059500061215     C/END-EXEC
059600061215     C
059700061215     C/EXEC SQL
059800061215     C+ Fetch next from C3 into :DS_WST94_TOTG
059900061215     C/END-EXEC
060000061215     C
060100061215     C/EXEC SQL
060200061215     C+ close C3
060300061215     C/END-EXEC
060400061220     C*
060500061220     C* Valorizzo i dati in stampa relativi al giro
060600070202     C                   clear                   stpcgipty
060700061215     C                   eval      stpgiro   = det_cgi
060800070202     C* - decodifica giro e priorità
060900070125     C                   if        det_cgi <> *blanks
061000070215     C                   eval      stpcgipty = %editc(det_pty:'4')
061100070125     C                   clear                   fidg09ds
061200070125     C                   eval      d09iop0 = '001'
061300070202     C                   eval      d09ifgs = det_fgs
061400070125     C                   eval      d09idat = datcor
061500070125     C                   eval      d09icgi = det_cgi
061600070709     C                   eval      d09itug = 'C'
061700070125     C                   movel(p)  fidg09ds      KPJBU
061800070125     C                   call      'FIDG09R'
061900070125     C                   parm                    KPJBA
062000070125     C                   movel(p)  KPJBU         fidg09ds
062100070125     C                   if        d09oerr = *blanks
062200120403     C                   eval      dgrgflo  = d09oflo
062201120403     C                   eval      stpfgm   = §grgfgm
062202120411     C                   eval      stpcsto  = §grgcsto
062203120403     C                   eval      stpain   = d09oain
062300070125     C                   eval      stpgirod = d09odes
062400070125     C                   else
062500120403     C                   eval      stpfgm   = *all'*'
062501120411     C                   eval      stpcsto  = *all'*'
062502120403     C                   eval      stpain   = *all'*'
062600070125     C                   eval      stpgirod = *all'*'
062700070125     C                   endif
062800070125     C*
062900070125     C                   else
063000070215     C                   eval      stpain    = *blanks
063100070215     C                   eval      stpgirod  = '*** non attribuito ***'
063200070215     C                   eval      stpcgipty = '--'
063300070125     C                   endif
063400070202     C*
063500070202     C                   exsr      valdet
063600061218     C*
063700061218     C* Gestisco salto pagina
063800061218     C                   if        *in05 = *on
063900061222     C   60              write     st94ri
064000061222     C   60              write     st94er
064100061218     C                   write     st94te
064200061222     C                   seton                                        60
064300061220     C  N40              write     st94dte
064400061220     C  N40              write     st94ri
064500061218     C                   setoff                                       05
064600061218     C*
064700061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
064800061220     C                   if        *in40 = *on
064900061220     C                   write     st94ri
065000061220     C                   write     st94dtd
065100061220     C                   write     st94ri
065200070202     C                   write     st94gtd
065300070202     C                   write     st94ri
065400061218     C                   endif
065500061218     C                   endif
065600061219     C*
065700061219     C* Se richiesta stampa in dettaglio lascio 1 riga vuota ad ogni cambio giro
065800061220     C                   if        *in40 = *on
065900061219     C   51              write     st94sp
066000061219     C                   endif
066100061218     C*
066200061218     C* Quindi stampo riga totali x distinta/giro
066300070202     C                   if        *in40 = *off
066400070202     C                   if        *in51 = *off
066500070202     C                   write     st94dt
066600070202     C                   seton                                        51
066700070202     C                   exsr      valdet
066800070202     C                   write     st94dtg
066900070202     C                   else
067000070202     C                   write     st94dtg
067100070202     C                   endif
067200070202     C                   endif
067300070202     C*
067400070202     C   40              write     st94ri
067500070202     C   40              write     st94dtd
067600070202     C   40              write     st94ri
067700070202     C   40              write     st94gtd
067800070202     C   40              write     st94ri
067900061218     C                   seton                                        51
068000061215     C*
068100061215     C* Ad ogni rottura d giro reperisco i totali bolle, colli. peso, volume x distinta/giro
068200061219     C                   dow       sqlcod = *zeros   AND
068300061219     C                             DET_NFV = SAV_NFV AND
068400061215     C                             DET_CGI = SAV_CGI
068500061215     C*
068600061215     C* Se scelta stampa anche dettaglio => provvedo
068700061220     C                   if        *in40 = *on
068800061222     C                   eval      stdsped = %editc(det_aas:'4') +
068900061220     C                                       ' ' +
069000070116     C                                       %trim(%editw(det_lnp:'0   ')) +
069100061220     C                                       ' ' +
069200061222     C                                       %editc(det_nrs:'4') +
069300061220     C                                       ' ' +
069400061222     C                                       %editc(det_nsp:'4')
069500070215     C                   if        det_tgi = 'M'
069600070215     C                   eval      stdtgi = 'M'
069700070215     C                   else
069800070215     C                   eval      stdtgi = *blanks
069900070215     C                   endif
070000061220     C                   eval      stdrsd = det_rsd
070100061220     C                   eval      stdind = det_ind
070200061220     C                   eval      stdloc = det_lod
070300061220     C                   eval      stdcap = det_cad
070400061221     C                   eval      stdprd = det_prd
070500061220     C***                eval      stdznc = det_znc
070600070125     C                   eval      stdncl =
070700070605     C                                       %subst(%editc(det_ncl:'2'):1:6)
070800070125     C                   eval      stdpkf =
070900061221     C                                       %subst(%editc(det_pes:'2'):3:7)
071000070125     C                   eval      stdvlf = %editc(det_vol:'2')
071100061222     C*
071200061222     C* Gestisco eventuale errore "TELLUS"
071300061222     C                   setoff                                       53
071400070102     C                   if        det_cgi =  *blanks AND
071500070102     C                             det_err <> '10'
071600061222     C                   seton                                        53
071700070102     C                   if        det_err = *blanks
071800070102     C                   eval      stderr = '(' + %subst(det_lno:1:2) + ')'
071900061222     C                   else
072000070102     C                   eval      stderr = '(' + det_err + ')'
072100061222     C                   endif
072200061222     C                   endif
072300061218     C*
072400061218     C* Gestisco salto pagina
072500061218     C                   if        *in05 = *on
072600061222     C   60              write     st94ri
072700061222     C   60              write     st94er
072800061218     C                   write     st94te
072900061222     C                   seton                                        60
073000061220     C  N40              write     st94dte
073100061220     C  N40              write     st94ri
073200061218     C                   setoff                                       05
073300061218     C*
073400061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
073500061220     C                   if        *in40 = *on
073600061220     C                   write     st94ri
073700061220     C                   write     st94dtd
073800061220     C                   write     st94ri
073900070202     C                   write     st94gtd
074000070202     C                   write     st94ri
074100061218     C                   endif
074200061218     C                   endif
074300061215     C*
074400061215     C* Stampo riga d dettaglio bolle
074500061215     C                   write     st94dd
074600061215     C                   endif
074700060516     C*
074800061214     C* Proseguo con la lettura del recordset
074900061214     C/EXEC SQL
075000061215     C+ Fetch next from C1 into :DS_WST94_DET
075100061214     C/END-EXEC
075200061214     C                   enddo
075300061215     C*
075400061215     C                   enddo
075500070117     C*
075600070117     C* Totalizzo le bolle
075700070117     C                   eval      wTotBolle = wTotBolle + totd_bol
075800061215     C*
075900061215     C                   enddo
076000060516     C*
076100060516     C/EXEC SQL
076200061215     C+ close C1
076300060516     C/END-EXEC
076400070117     C*
076500070117     C                   evalr     stototbol = %editc(wTotBolle:'2')
076600061215     C*
076700061215     C* Chiusura printer-file
076800061220     C                   write     st94ri
076900070117     C                   write     st94tt
077000070117     C                   write     st94ri
077100061222     C                   write     st94er
077200061220     C                   write     st94bk
077300061220     C                   write     st94bk
077400061215     C                   write     st94fi
077500061215     C                   close     TIST94P
077600060516     C*
077700060516     C                   ENDSR
077800070215     C*------------------------------------------------------------------------*
077900070215     C* STA_NDC_PTY - Routine di stampa x distita/priorità (solo totali)
078000070215     C*------------------------------------------------------------------------*
078100070215     C     STA_NDC_PTY   BEGSR
078200070215     C*
078300070215     C* Apertura printer-file
078400070215     C                   open      TIST94P
078500070215     C*
078600070215     C* Valorizzo titolo tabulato
078700070215     C                   eval      stttes1 = '*** VERIFICA ABBINAMENTI ' +
078800070215     C                                       'DISTINTE/PRIORITA ***'
078900070215     C                   eval      stttes2 = ' (TOTALI) '
079000070215     C*
079100070215     C* Valorizzo i paramteri d lancio
079200120404     C                   eval      stppar = 'Fil.: ' +
079300070215     C                                       %trim(%editw(In_FGS:'0   '))
079400070215     C                   if        In_NFV <> *zeros
079500070215     C                   eval      stppar = %trim(stppar) + ' '+
079600070215     C                                      'PARAMETRI: tutti le bolle nella ' +
079700070215     C                                      'distinta '                        +
079800070215     C                                       %trim(%editw(In_FGS:'0   '))      +
079900070215     C                                       ' '                               +
080000070215     C                                       %trim(%editc(In_NFV:'4'))
080100070215     C                   else
080200070215     C                   if        In_DFV <> *zeros
080300070215     C                   eval      stppar = %trim(stppar) + ' '+
080400070215     C                                      'PARAMETRI: tutti le bolle nelle ' +
080500070215     C                                      'distinte in data '                +
080600070215     C                                      %editw(In_DFV_stp:'0  /  /    ')
080700070215     C                   else
080800070215     C                   eval      stppar = %trim(stppar) + ' '+
080900070215     C                                      'PARAMETRI: tutti le bolle non ' +
081000070215     C                                      'inserite in nessuna distinta'
081100070215     C                   endif
081200070215     C                   endif
081300070215     C*
081400070215     C* Se scelta stampa nn dettaglio stampo subito la testata
081500070215     C                   if        *in40 = *off
081600070215     C                   write     st94te
081700070215     C                   seton                                        60
081800070215     C                   write     st94dte
081900070215     C                   write     st94ri
082000070215     C                   setoff                                       05
082100070215     C                   endif
082200070215     C*
082300070215     C/EXEC SQL
082400070215     C+ declare C10 cursor for
082500070215     C+ select * from wfst940f
082600070215     C+ order by wst94nfv, wst94pty
082700070215     C+ for read only
082800070215     C/END-EXEC
082900070215     C
083000070215     C/EXEC SQL
083100070215     C+ open C10
083200070215     C/END-EXEC
083300070215     C
083400070215     C/EXEC SQL
083500070215     C+ Fetch next from C10 into :DS_WST94_DET
083600070215     C/END-EXEC
083700070215     C*
083800070215     C* Se reperito qualcosa => analizzo
083900070215     C                   dow       sqlcod = *zeros
084000070215     C*
084100070215     C* Valorizzo i dati in stampa relativi alla distinta
084200070215     C*
084300070215     C* - codice distinta
084400070215     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
084500070215     C                                       ' ' +
084600070215     C                                       %trim(%editc(det_nfv:'4'))
084700070215     C* - autotrasportatore
084800070215     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
084900070215     C                                       ' ' +
085000070215     C                                       det_rsc
085002120403     c                   if        %subst(det_flr: 9: 7) <> ' '
085003120404     c                   eval      det_mcp = %subst(det_flr: 9: 6)
085004120403     c                   move      det_mcp       stpmcp
085005120403     c                   else
085006120403     c                   clear                   stpmcp
085007120403     c                   end
085100070215     C* - data distinta
085200070215     C                   z-add     det_dfv       G08INV
085300070215     C                   z-add     *ZEROS        G08DAT
085400070215     C                   movel     '3'           G08ERR
085500070215     C                   call      'XSRDA8'
085600070215     C                   parm                    WLBDA8
085700070215     C                   z-add     G08DAT        stpdat
085800070215     C* - prestazione distinta
085900070215     C                   if        det_fpp = 'M'
086000070215     C                   eval      stpprest = 'MAT.'
086100070215     C                   endif
086200070215     C                   if        det_fpp = 'P'
086300070215     C                   eval      stpprest = 'POM.'
086400070215     C                   endif
086500070215     C*
086600070215     C* Inizializzo l'indicatore d nuova distinta
086700070215     C                   setoff                                       51
086800070215     C*
086900070215     C* Salvo la distinta corrente
087000070215     C                   eval      SAV_NFV = DET_NFV
087100070215     C*
087200070215     C/EXEC SQL
087300070215     C+ declare C11 cursor for
087400070215     C+ select wst94nfv, count(*),
087500070215     C+ decimal(sum(wst94ncl), 9, 0),
087600070215     C+ decimal(sum(wst94pes), 9, 0),
087700070215     C+ decimal(sum(wst94vol), 9, 2)
087800070215     C+ from wfst940f
087900070215     C+ where wst94nfv = :SAV_NFV
088000070215     C+ group by wst94nfv
088100070215     C+ for read only
088200070215     C/END-EXEC
088300070215     C
088400070215     C/EXEC SQL
088500070215     C+ open C11
088600070215     C/END-EXEC
088700070215     C
088800070215     C/EXEC SQL
088900070215     C+ Fetch next from C11 into :DS_WST94_TOTD
089000070215     C/END-EXEC
089100070215     C
089200070215     C/EXEC SQL
089300070215     C+ close C11
089400070215     C/END-EXEC
089500070215     C*
089600070215     C* Ad ogni rottura d distinta reperisco il totale bolle in distinta
089700070215     C                   dow       sqlcod = *zeros AND
089800070215     C                             DET_NFV = SAV_NFV
089900070215     C*
090000070215     C* Salvo il giro corrente
090100070215     C                   eval      SAV_PTY = DET_PTY
090200070215     C*
090300070215     C/EXEC SQL
090400070215     C+ declare C12 cursor for
090500070215     C+ select wst94nfv, wst94pty, count(*),
090600070215     C+ decimal(sum(wst94ncl), 9, 0),
090700070215     C+ decimal(sum(wst94pes), 9, 0),
090800070215     C+ decimal(sum(wst94vol)+0.005, 9, 2)
090900070215     C+ from wfst940f
091000070215     C+ where wst94nfv = :SAV_NFV and wst94pty = :SAV_PTY
091100070215     C+ group by wst94nfv, wst94pty
091200070215     C+ for read only
091300070215     C/END-EXEC
091400070215     C
091500070215     C/EXEC SQL
091600070215     C+ open C12
091700070215     C/END-EXEC
091800070215     C
091900070215     C/EXEC SQL
092000070215     C+ Fetch next from C12 into :DS_WST94_TOTG
092100070215     C/END-EXEC
092200070215     C
092300070215     C/EXEC SQL
092400070215     C+ close C12
092500070215     C/END-EXEC
092600070215     C*
092700070215     C* Valorizzo i dati in stampa relativi al giro
092800070215     C                   clear                   stpcgipty
092900070215     C                   eval      stpgiro   = *blanks
093000070215     C* - decodifica giro e priorità
093100070215     C                   if        det_cgi <> *blanks
093200070215     C                   eval      stpcgipty = %editc(det_pty:'4')
093300070215     C                   else
093400070215     C                   eval      stpcgipty = '--'
093500070215     C                   endif
093600070215     C*
093700070215     C                   exsr      valdet
093800070215     C*
093900070215     C* Gestisco salto pagina
094000070215     C                   if        *in05 = *on
094100070215     C   60              write     st94ri
094200070215     C   60              write     st94er
094300070215     C                   write     st94te
094400070215     C                   seton                                        60
094500070215     C  N40              write     st94dte
094600070215     C  N40              write     st94ri
094700070215     C                   setoff                                       05
094800070215     C                   endif
094900070215     C*
095000070215     C* Quindi stampo riga totali x distinta/priorità
095100070215     C                   if        *in51 = *off
095200070215     C                   write     st94dt
095300070215     C                   seton                                        51
095400070215     C                   exsr      valdet
095500070215     C                   write     st94dtg
095600070215     C                   else
095700070215     C                   write     st94dtg
095800070215     C                   endif
095900070215     C                   seton                                        51
096000070215     C*
096100070215     C* Ad ogni rottura d giro reperisco i totali bolle, colli. peso, volume x distinta/giro
096200070215     C                   dow       sqlcod = *zeros   AND
096300070215     C                             DET_NFV = SAV_NFV AND
096400070215     C                             DET_PTY = SAV_PTY
096500070215     C*
096600070215     C* Proseguo con la lettura del recordset
096700070215     C/EXEC SQL
096800070215     C+ Fetch next from C10 into :DS_WST94_DET
096900070215     C/END-EXEC
097000070215     C                   enddo
097100070215     C*
097200070215     C                   enddo
097300070215     C*
097400070215     C* Totalizzo le bolle
097500070215     C                   eval      wTotBolle = wTotBolle + totd_bol
097600070215     C*
097700070215     C                   enddo
097800070215     C*
097900070215     C/EXEC SQL
098000070215     C+ close C10
098100070215     C/END-EXEC
098200070215     C*
098300070215     C                   evalr     stototbol = %editc(wTotBolle:'2')
098400070215     C*
098500070215     C* Chiusura printer-file
098600070215     C                   write     st94ri
098700070215     C                   write     st94tt
098800070215     C                   write     st94ri
098900070215     C                   write     st94er
099000070215     C                   write     st94bk
099100070215     C                   write     st94bk
099200070215     C                   write     st94fi
099300070215     C                   close     TIST94P
099400070215     C*
099500070215     C                   ENDSR
099600070202     C*------------------------------------------------------------------------*
099700070202     C* VALDET - Routine valorizzazione dettaglio in stampa
099800070202     C*------------------------------------------------------------------------*
099900070202     C     VALDET        BEGSR
100000070202     C*
100100070202     C* Se sintetica....
100200070202     C                   if        *in40 = *off
100300070202     C   51              evalr     stptotbol =
100400070605     C                             %subst(%editc(totg_bol:'1'):7:5)
100500070202     C   51              eval      stpperc   = totg_bol * 100 / totd_bol
100600070202     C   51              evalr     stptotncl =
100700070202     C                             %subst(%editc(totg_ncl:'1'):7:5)
100800070202     C   51              evalr     stptotpkf =
100900070202     C                             %subst(%editc(totg_pes:'1'):6:6)
101000070202     C   51              evalr     stptotvlf =
101100070202     C                             %subst(%editc(totg_vol:'1'):8:5)
101200070202     C*
101300070202     C  N51              evalr     stptotbol =
101400070605     C                             %subst(%editc(totd_bol:'1'):7:5)
101500070202     C  N51              eval      stpperc   = *zeros
101600070202     C  N51              evalr     stptotncl =
101700070202     C                             %subst(%editc(totd_ncl:'1'):7:5)
101800070202     C  N51              evalr     stptotpkf =
101900070202     C                             %subst(%editc(totd_pes:'1'):6:6)
102000070202     C  N51              evalr     stptotvlf =
102100070202     C                             %subst(%editc(totd_vol:'1'):8:5)
102200070202     C                   else
102300070202     C                   evalr     stptotbol =
102400070605     C                             %subst(%editc(totg_bol:'1'):7:5) + '/' +
102500070605     C                             %subst(%editc(totd_bol:'1'):7:5)
102600070202     C                   eval      stpperc   = totg_bol * 100 / totd_bol
102700070202     C                   evalr     stptotncl =
102800070202     C                             %subst(%editc(totg_ncl:'1'):7:5) + '/' +
102900070202     C                             %subst(%editc(totd_ncl:'1'):7:5)
103000070202     C                   evalr     stptotpkf =
103100070202     C                             %subst(%editc(totg_pes:'1'):6:6) + '/' +
103200070202     C                             %subst(%editc(totd_pes:'1'):6:6)
103300070202     C                   evalr     stptotvlf =
103400070202     C                             %subst(%editc(totg_vol:'1'):8:5) + '/' +
103500070202     C                             %subst(%editc(totd_vol:'1'):8:5)
103600070202     C                   endif
103700070202     C*
103800070202     C                   ENDSR
103900061220     C*------------------------------------------------------------------------*
104000061220     C* STA_CGI - Routine di stampa x giro
104100061220     C*------------------------------------------------------------------------*
104200061220     C     STA_CGI       BEGSR
104300061220     C*
104400061220     C* Apertura printer-file
104500061220     C                   open      TIST94P
104600061220     C*
104700061220     C* Valorizzo titolo tabulato
104800070125     C                   eval      stttes1 = '      *** VERIFICA ASSORBIMENTO '+
104900070117     C                                       'GIRI ***'
105000070201     C                   if        In_LIV = 'S'
105100070201     C                   eval      stttes2 = '(SINTETICA)'
105200070201     C                   else
105300070201     C                   eval      stttes2 = '(DETTAGLIO)'
105400070201     C                   endif
105500061222     C*
105600061222     C* Valorizzo i paramteri d lancio
105700120404     C                   eval      stppar = 'Fil.: ' +
105800070116     C                                       %trim(%editw(In_FGS:'0   '))
105900070201     C                   if        In_DFV <> *zeros
106000070201     C                   eval      stppar = %trim(stppar) + ' '+
106100070201     C                                      'PARAMETRI: tutti le bolle nelle ' +
106200070201     C                                      'distinte in data '                +
106300070201     C                                      %editw(In_DFV_stp:'0  /  /    ')
106400070201     C                   else
106500070201     C                   eval      stppar = %trim(stppar) + ' '+
106600070201     C                                      'PARAMETRI: tutti le bolle non ' +
106700070201     C                                      'inserite in nessuna distinta'
106800070201     C                   endif
106900061220     C*
107000061220     C* Se scelta stampa nn dettaglio stampo subito la testata
107100061220     C                   if        *in40 = *off
107200061220     C                   write     st94te
107300061222     C                   seton                                        60
107400061220     C                   write     st94gte
107500061220     C                   write     st94ri
107600061222     C                   setoff                                       05
107700061220     C                   endif
107800061220     C*
107900061220     C/EXEC SQL
108000061220     C+ declare C7 cursor for
108100061220     C+ select * from wfst940f
108200061221     C+ order by wst94cgi, wst94prd, wst94lod,
108300061221     C+ wst94ind, wst94cad, wst94rsd
108400061220     C+ for read only
108500061220     C/END-EXEC
108600061220     C
108700061220     C/EXEC SQL
108800061220     C+ open C7
108900061220     C/END-EXEC
109000061220     C
109100070202     C/EXEC SQL
109200070202     C+ Fetch next from C7 into :DS_WST94_DET
109300070202     C/END-EXEC
109400070202     C
109500061220     C*
109600061220     C* Se reperito qualcosa => analizzo
109700070202     C                   dow       sqlcod = *zeros
109800061220     C*
109900061220     C* Inizializzo l'indicatore d nuovo giro
110000061220     C                   setoff                                       51
110100061220     C*
110200061220     C* Se scelta stampa anche dettaglio ad ogni giro salto pagina
110300061220     C                   if        *in40 = *on
110400061222     C   60              write     st94ri
110500061222     C   60              write     st94er
110600061220     C                   write     st94te
110700061222     C                   seton                                        60
110800061222     C                   setoff                                       05
110900061220     C                   endif
111000061220     C*
111100061220     C* Salvo il giro corrente
111200061220     C                   eval      SAV_CGI = DET_CGI
111300061220     C*
111400061220     C/EXEC SQL
111500061220     C+ declare C8 cursor for
111600061220     C+ select min(wst94nfv), wst94cgi, count(*),
111700061221     C+ decimal(sum(wst94ncl), 9, 0), decimal(sum(wst94pes), 9, 0),
111800061221     C+ decimal(sum(wst94vol), 9, 2) from wfst940f
111900061220     C+ where wst94cgi = :SAV_CGI
112000061221     C+ group by wst94cgi
112100061220     C+ for read only
112200061220     C/END-EXEC
112300061220     C
112400061220     C/EXEC SQL
112500061220     C+ open C8
112600061220     C/END-EXEC
112700061220     C
112800070202     C
112900070202     C*
113000061220     C/EXEC SQL
113100061220     C+ Fetch next from C8 into :DS_WST94_TOTG
113200061220     C/END-EXEC
113300061220     C
113400061220     C/EXEC SQL
113500061220     C+ close C8
113600061220     C/END-EXEC
113601120403     c* tottalizzo le spedizioni micro > 5 kg e 0,023 volume
113602120404     c                   clear                   sqlcod
113603120404     c                   clear                   DS_WST94_TGM
113605120404     c                   if        not *in40
113606120403     C/EXEC SQL
113607120403     C+ declare C4 cursor for
113608120404     C+ select min(wst94nfv), wst94cgi, count(*),
113609120404     C+ decimal(sum(wst94ncl), 9, 0), decimal(sum(wst94pes), 9, 0),
113610120404     C+ decimal(sum(wst94vol), 9, 2) from wfst940f
113611120404     C+ where wst94cgi = :SAV_CGI
113612120404     C+ and (wst94pes <= :§LIMPES  or wst94vol <= :§LIMvol )
113613120404     C+ group by wst94cgi
113619120403     C+ for read only
113620120403     C/END-EXEC
113621120403     C
113622120403     C/EXEC SQL
113623120403     C+ open C4
113624120403     C/END-EXEC
113625120403     C
113626120403     C/EXEC SQL
113627120403     C+ Fetch next from C4 into :DS_WST94_TGm
113628120403     C/END-EXEC
113629120403     C
113630120403     C/EXEC SQL
113631120403     C+ close C4
113632120403     C/END-EXEC
113633120404     c                   end
113634120404     c                   clear                   sqlcod
113700061220     C*
113800061220     C* Valorizzo i dati in stampa relativi al giro
113900061220     C*
114000061220     C* - codice giro
114100061220     C                   eval      stpgiro   = det_cgi
114200070125     C* - decodifica giro
114300070125     C                   if        det_cgi <> *blanks
114400070125     C                   clear                   fidg09ds
114500070125     C                   eval      d09iop0 = '001'
114600070202     C                   eval      d09ifgs = det_fgs
114700070125     C                   eval      d09idat = datcor
114800070125     C                   eval      d09icgi = det_cgi
114900070709     C                   eval      d09itug = 'C'
115000070125     C                   movel(p)  fidg09ds      KPJBU
115100070125     C                   call      'FIDG09R'
115200070125     C                   parm                    KPJBA
115300070125     C                   movel(p)  KPJBU         fidg09ds
115400070125     C                   if        d09oerr = *blanks
115401120403     C                   eval      dgrgflo  = d09oflo
115402120403     C                   eval      stpfgm   = §grgfgm
115403120411     C                   eval      stpcsto  = §grgcsto
115500070205     C                   eval      stpain   = d09oain
115600070205     C                   eval      stpgirod = d09odes
115700070125     C                   else
115800120403     C                   eval      stpfgm   = *all'*'
115801120411     C                   eval      stpcsto  = *all'*'
115802120403     C                   eval      stpain   = *all'*'
115900070205     C                   eval      stpgirod = *all'*'
116000070125     C                   endif
116100070125     C*
116200070125     C                   else
116300070205     C                   eval      stpain   = *blanks
116400070125     C                   eval      stpgirod = '*** non attribuito ***'
116500070125     C                   endif
116501120404     C* - totali
116502120404     C                   evalr     stptotbol =
116503120404     C                             %subst(%editc(totg_bol:'1'):7:5)
116504120404     C                   evalr     stptotncl =
116505120404     C                             %subst(%editc(totg_ncl:'1'):7:5)
116506120404     C                   evalr     stptotpkf =
116507120404     C                             %subst(%editc(totg_pes:'1'):6:6)
116508120404     C                   evalr     stptotvlf =
116509120404     C                             %subst(%editc(totg_vol:'1'):8:5)
116510120404     C                   evalr     stptotbolm=
116511120404     C                             %subst(%editc(tgm_bol:'2'):7:5)
116512120404     C                   evalr     stptotnclm=
116513120404     C                             %subst(%editc(tgm_ncl:'2'):7:5)
116514120404     C                   evalr     stptotpkfm=
116515120404     C                             %subst(%editc(tgm_pes:'2'):6:6)
116516120404     C                   evalr     stptotvlfm=
116517120404     C                             %subst(%editc(tgm_vol:'2'):8:5)
117500061220     C*
117600061220     C* Gestisco salto pagina
117700061220     C                   if        *in05 = *on
117800061222     C   60              write     st94ri
117900061222     C   60              write     st94er
118000061220     C                   write     st94te
118100061222     C                   seton                                        60
118200061220     C  N40              write     st94gte
118300070125     C   40              write     st94gtt
118400070125     C   40              write     st94ri
118500070125     C   40              write     st94gtd
118600061220     C                   write     st94ri
118700061220     C                   setoff                                       05
118800061220     C                   endif
118900061220     C*
119000061220     C* Stampo il giro corrente
119100070125     C   40              write     st94gtt
119200070125     C   40              write     st94ri
119300070125     C   40              write     st94gtd
119400061220     C   40              write     st94ri
119500061220     C  N40              write     st94gt
119600061220     C  N40              setoff                                       51
119700061220     C*
119800061220     C* Ad ogni rottura d giro se richiesto stampa il dettaglio bolle
119900061220     C                   dow       sqlcod = *zeros AND
120000061220     C                             DET_CGI = SAV_CGI
120100061220     C*
120200061220     C* Gestisco salto pagina
120300061220     C                   if        *in05 = *on
120400061222     C   60              write     st94ri
120500061222     C   60              write     st94er
120600061220     C                   write     st94te
120700061222     C                   seton                                        60
120800061220     C  N40              write     st94gte
120900070125     C   40              write     st94gtt
121000070125     C   40              write     st94ri
121100070125     C   40              write     st94gtd
121200061220     C                   write     st94ri
121300061220     C                   setoff                                       05
121400061220     C                   endif
121500061220     C*
121600061220     C* Se scelta stampa anche dettaglio => provvedo
121700061220     C                   if        *in40 = *on
121800061222     C                   eval      stdsped = %editc(det_aas:'4') +
121900061220     C                                       ' ' +
122000070116     C                                       %trim(%editw(det_lnp:'0   ')) +
122100061220     C                                       ' ' +
122200061222     C                                       %editc(det_nrs:'4') +
122300061220     C                                       ' ' +
122400061222     C                                       %editc(det_nsp:'4')
122500070215     C                   if        det_tgi = 'M'
122600070215     C                   eval      stdtgi = 'M'
122700070215     C                   else
122800070215     C                   eval      stdtgi = *blanks
122900070215     C                   endif
123000061220     C                   eval      stdrsd = det_rsd
123100061220     C                   eval      stdind = det_ind
123200061220     C                   eval      stdloc = det_lod
123300061220     C                   eval      stdcap = det_cad
123400061221     C                   eval      stdprd = det_prd
123500061220     C***                eval      stdznc = det_znc
123600070125     C                   eval      stdncl =
123700070605     C                                       %subst(%editc(det_ncl:'2'):1:6)
123800070125     C                   eval      stdpkf =
123900061221     C                                       %subst(%editc(det_pes:'2'):3:7)
124000070125     C                   eval      stdvlf = %editc(det_vol:'2')
124100070102     C*
124200070102     C* Gestisco eventuale errore "TELLUS"
124300070102     C                   setoff                                       53
124400070102     C                   if        det_cgi =  *blanks AND
124500070102     C                             det_err <> '10'
124600070102     C                   seton                                        53
124700070102     C                   if        det_err = *blanks
124800070102     C                   eval      stderr = '(' + %subst(det_lno:1:2) + ')'
124900070102     C                   else
125000070102     C                   eval      stderr = '(' + det_err + ')'
125100070102     C                   endif
125200070102     C                   endif
125300061220     C*
125400061220     C* Stampo riga d dettaglio bolle
125500061220     C                   write     st94dd
125600061220     C                   endif
125700070202     C/EXEC SQL
125800070202     C+ Fetch next from C7 into :DS_WST94_DET
125900070202     C/END-EXEC
126000070202     C*
126100061220     C                   enddo
126200070117     C*
126300070117     C* Totalizzo le bolle
126400070117     C                   eval      wTotBolle = wTotBolle + totg_bol
126401120403     C                   eval      wTotBollem= wTotBollem+ tgm_bol
126500061220     C*
126600061220     C                   enddo
126700061220     C*
126800061220     C/EXEC SQL
126900061220     C+ close C7
127000061220     C/END-EXEC
127100070117     C*
127200070117     C                   evalr     stototbol = %editc(wTotBolle:'2')
127201120403     C                   evalr     stototbolm= %editc(wTotBollem:'2')
127300070117     C*
127400070117     C* Chiusura printer-file
127500070117     C                   write     st94ri
127600070117     C                   write     st94tt
127700061220     C                   write     st94ri
127800061222     C                   write     st94er
127900061220     C                   write     st94bk
128000061220     C                   write     st94bk
128100061220     C                   write     st94fi
128200061220     C                   close     TIST94P
128300061220     C*
128400061220     C                   ENDSR
128500061215     C*--------------------------------------------------------------------------------------------*
128600061215     C* REPERISCE I DATI UTENTE
128700061215     C*--------------------------------------------------------------------------------------------*
128800061215     C     REPDATIUTE    BEGSR
128900061215     C*
129000061215     C* INIZIALIZZA VARIABILI DI WRK
129100061215     C                   CLEAR                   TIBS34DS
129200061215     C                   CLEAR                   AZUTEDS
129300061215     C                   CLEAR                   DDATIUTE
129400061215     C*
129500061215     C     *DTAARA       DEFINE    §azute        azuteds
129600061215     C     *DTAARA       DEFINE    §datiute      ddatiute
129700061215     C                   IN(E)     *DTAARA
129800061215     C                   IF        %Error
129900061215     c                   EVAL      I34Tla = 'L'
130000061215     C                   CALL      'TIBS34R'
130100061215     C                   PARM                    Tibs34Ds
130200061215     C                   IN        *DTAARA
130300061215     C                   ENDIF
130400061215     C*
130500061215     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
130600061215     C                   MOVEL(P)  RSUT          DSFIRS           20
130700061215     C*
130800061215     C                   ENDSR
130900070202     c*------------------------------------------------------------------
131000070202     c* Esclusione delle spedizioni da non passare a Tellus (presa da *pgm Elisa FNLRL1R)
131100070202     c*------------------------------------------------------------------
131200070202     c     ESCLUDI       BEGSR
131300070202     c*
131400070202     c                   clear                   Escbolla
131500070202     c*
131600070202     c* 1) Escludo bolle già consegnate
131700070202     c                   if        arbdcm > 0
131800070202     c                   eval      Escbolla='S'
131900070202     c                   goto      Endescludi
132000070202     c                   endif
132100070202     c*
132200070205     c* 2) Spedizioni già in distinta
132300070202     c* 3) Spedizioni in Fermo deposito
132400070202     c* 4) Spedizioni bloccate ("*" è per il cambio di porto)
132500070205     c                   if        (arbndc>0 and arbddc>0) or
132600070205     c                             arbffd<>' '             or
132700070205     c                             arbfbc='B' or arbfbc='*'
132800070202     c                   eval      Escbolla='S'
132900070202     c                   goto      Endescludi
133000070202     c                   endif
133100070202     c*
133200070202     c* 5) Tipo bolla di recupero
133300070202     c     arbcbo        lookup    REC                                    35
133400070202     c                   if        *in35
133500070202     c                   eval      Escbolla='S'
133600070202     c                   goto      Endescludi
133700070202     c                   endif
133800070202     c
133900070202     c* 6) con C.A. aperta
134000070202     c                   clear                   fidn12ds
134100070202     c                   eval      i12aas = arbaas
134200070202     c                   eval      i12lnp = arblnp
134300070202     c                   eval      i12nrs = arbnrs
134400070202     c                   eval      i12nsp = arbnsp
134500070202     c                   eval      i12fel = 'E'
134600070202     c                   eval      i12fan = 'E'
134700070202      *
134800070202     c                   call      'FIDN12R'
134900070202     c                   parm                    fidn12ds
135000070202      *
135100070202      * se non ci sono errori
135200070202     c                   if        o12err = ' '
135300070202      * se numero di CA trovate maggiore di zero
135400070202     c                   if        o12nca > 0
135500070202     c
135600070202     c                   z-add     *zeros        ii                2 0
135700070202     c     1             do        o12nca        ii
135800070202     c                   movea     skkey(ii)     dskey
135900070202     C                   Z-ADD     skdch(ii)     dctdch            8 0
136000070202     C                   movel     skcch(ii)     dctcch            2
136100070202      *
136200070202     C* -SE ESISTE C.A. APERTA  --> ESCO
136300070202     c                   If        dctdch = 0
136400070202     c                   eval      Escbolla='S'
136500070202     c                   leave
136600070202     c                   endif
136700070202     c
136800070202     c* Se c.a. chiusa con causale che non prevede consegna esco
136900070202     c                   if        dctcch<>*blanks
137000070202     c     dctcch        lookup    cchco                                  35
137100070202     c                   if        not *in35
137200070202     c                   eval      Escbolla='S'
137300070202     c                   leave
137400070202     c                   endif
137500070202     c                   endif
137600070202     c*
137700070202     C                   enddo
137800070202     c*
137900070202     c                   endif
138000070202     c                   endif
138100070202     C*
138200070202     C* -SE ESISTE C.A. APERTA  --> ESCO
138300070202    1C     Escbolla      IFEQ      'S'
138400070202     c                   goto      Endescludi
138500070202    1C                   ENDIF
138600070202     c*
138700070202     c* 7) Senza testare il record di giacenza, escludo tutte le bolle
138800070202     c*    con blocco G
138900070202    1c                   if        arbfbc='G'
139000070202     c                   eval      Escbolla='S'
139100070202     c                   goto      Endescludi
139200070202    3C                   endif
139300070202     C*
139400070202     c     Endescludi    ENDSR
139500070202     C*--------------------------------------------------------------------------------------------*
139600070202     C* CARTAB - Caricamento dati tabellati (logiche prese da *pgm Elisa FNLRL1R)
139700070202     C*--------------------------------------------------------------------------------------------*
139800070202     C     CARTAB        BEGSR
139900070202     C*
140000070202     c* Carico le causali chiusura c,a, che prevedono la consegna della merce
140100070202     c                   z-add     *zeros        xx                3 0
140200070202     C                   MOVEL     'CCH'         wTBEcod
140300070202     c     wTBEcod       chain     tntbe000                           31
140400070202      *
140500070202     c                   DOW       *in31 = *off
140600070202     c                   movel     TBEuni        DCCH
140700070202      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
140800070202     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
140900070202     c                             and TBEatb = *BLANKS  and  §cchccco= 'S'
141000070202     c                   add       1             xx
141100070202     c                   movel     TBEke1        cchco(XX)
141200070202     c                   endif
141300070202     c     wTBEcod       reade     tntbe000                               31
141400070202     c                   enddo
141500070202     c
141600070202     c* Carico tipi record di recupero
141700070202     C                   Z-ADD     1             KTBKUT
141800070202     C                   MOVE      '3A'          KTBCOD
141900070202     C                   CLEAR                   IY                4 0
142000070202     C     KEYtab_c      SETLL     TABEL00F
142100070202     C     KEYtab_c      READE     TABEL00F                               98
142200070202     C     *IN98         DOWEQ     *OFF
142300070202     C                   MOVEL     TBLUNI        DS3a
142400070202     C     §3arbl        IFEQ      'R'
142500070202     C                   ADD       1             IY
142600070202     C                   MOVEL     TBLKEY        rec(IY)
142700070202     C                   ENDIF
142800070202     C     KEYtab_c      READE     TABEL00F                               98
142900070202     C                   ENDDO
142901120403     c* valori spedizione micro
142902120404     c                   eval      tbecod='LIM'
142903120404     c                   eval      tbeke1='MICRO'
142904120403     C     KEYtbe        chain     Tntbe01l
142905120403     c                   if        %found(tntbe01l)
142906120403     c                   movel     tbeuni        dlimmicro
142907120403     c                   else
142908120403     c                   clear                   dlimmicro
142909120403     c                   end
143000070202     C*
143100070202     C                   ENDSR
143200070202     C*--------------------------------------------------------------------------------------------*
143300070202     C* CARICA I P.O. IN "lira" richiesta
143400070202     C*--------------------------------------------------------------------------------------------*
143500070202     C     CARLIRA       BEGSR
143600070202     C*
143700070202     C* CARICO TABELLA FILIALI GESTITE £6
143800070202     C                   clear                   trul06ds
143900070202     C                   move      '£6'          d06cod
144000070205     C                   movel     In_FGS        d06key
144100070202     C                   movel     trul06ds      kpjbu
144200070202     C                   call      'TRUL06R'
144300070202     C                   parm                    kpjba
144400070202     C                   movel     kpjbu         trul06ds
144500070202     C*
144600070202     C                   ENDSR
144700060515     C*--------------------------------------------------------------------------------------------*
144800060515     C* *inzsr - operazioni iniziali
144900060515     C*--------------------------------------------------------------------------------------------*
145000000000     C     *inzsr        BEGSR
145100060515     C*
145200060515     C* Ricevimento parametri
145300060515     C     *ENTRY        PLIST
145400061214     C                   PARM                    KPJBA
145500061214     C*
145600061214     C* Ridefinisco i parametri
145700061214     C                   MOVEL     KPJBU         PARAM
145800061220     C*
145900061220     C* Verifico subito se richiesta stampa in dettaglio o sintetica
146000070215     C                   setoff                                       40
146100070215     C                   setoff                                       54
146200070215     C                   if        In_LIV  = 'D'
146300070215     C                   seton                                        40
146400070215     C                   endif
146500070215     C                   if        In_LIV  = 'T'
146600070215     C                   seton                                        54
146700070215     C                   endif
146800070226     C                   if        In_PIG  = 'A'
146900070226     C                   eval      In_PIG_num = 1
147000070226     C                   else
147100070226     C                   eval      In_PIG_num = 2
147200070226     C                   endif
147300070125     C*
147400070125     C* CALCOLA LA DATA CORRENTE
147500070125     C                   time                    wn14             14 0
147600070125     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
147700070125     C                   z-add     wn8           g08dat
147800070125     C                   z-add     *zeros        g08inv
147900070125     C                   movel     '0'           g08err
148000070125     C                   call      'XSRDA8'
148100070125     C                   parm                    wlbda8
148200070125     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
148300070202     C*
148400070202     C* Definizione chiavi d lettura
148500070202     C* - FNARB01L
148600070202     C     KEYarb01_c    KLIST
148700070202     C                   KFLD                    DET_AAS
148800070202     C                   KFLD                    DET_LNP
148900070202     C                   KFLD                    DET_NRS
149000070202     C                   KFLD                    DET_NSP
149100070202     C* - TABEL00F
149200070202     C     KEYtab_c      KLIST
149300070202     C                   KFLD                    KTBKUT
149400070202     C                   KFLD                    KTBCOD
149401120403     C* - Tntbe01l
149402120403     C     KEYtbe        KLIST
149403120403     C                   KFLD                    tbecod
149404120403     C                   KFLD                    tbeke1
149500060515     C*
149600060515     C                   ENDSR
