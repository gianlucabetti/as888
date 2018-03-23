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
003500120403     D dgrgflo       E DS
003600120403     D dlimmicro     E DS
003700061215     D*------------------
003800061215     D* DS PASSAGGIO PARAMETRI
003900061215     D*------------------
004000061215     D PARAM           DS
004100061215     D  In_FGS                        3  0
004200061215     D  In_DFV                        8  0
004300061215     D  In_NFV                        7  0
004400070201     D  In_LIV                        1
004500061220     D  In_TIP                        2
004600070223     D  In_PIG                        1
004700061215     D*------------------
004800061215     D* DS DI WRK
004900061215     D*------------------
005000061215     D DS_WST94_DET    DS
005100061215     D  DET_FGS                       3  0
005200061215     D  DET_NPG                       1
005300061215     D  DET_NFV                       7  0
005400061215     D  DET_DFV                       8  0
005500061215     D  DET_FPP                       1
005600061215     D  DET_PDR                       7  0
005700061215     D  DET_TIP                       1
005800061215     D  DET_RSC                      35
005900120403     D  DET_FLR                      30
006000061215     D  DET_CGI                      10
006100070215     D  DET_TGI                       1
006200070129     D  DET_PTY                       2  0
006300061222     D  DET_LNO                       3
006400061222     D  DET_ERR                       2
006500061215     D  DET_AAS                       4  0
006600061215     D  DET_LNP                       3  0
006700061215     D  DET_NRS                       2  0
006800061215     D  DET_NSP                       7  0
006900070202     D  DET_LNA                       3  0
007000061215     D  DET_NCL                       5  0
007100061221     D  DET_PES                       7  1
007200061221     D  DET_VOL                       5  3
007300061220     D  DET_RSD                      35
007400061220     D  DET_IND                      35
007500061220     D  DET_LOD                      35
007600061220     D  DET_CAD                      35
007700061221     D  DET_PRD                       2
007800061220     D  DET_ZNC                       2  0
007900120404     D  DET_MCP        s              6
008000061215     D*
008100061215     D DS_WST94_TOTD   DS
008200061215     D  TOTD_NFV                      7  0
008300061218     D  TOTD_BOL                      9  0
008400061219     D  TOTD_NCL                      9  0
008500061221     D  TOTD_PES                      9  0
008600061221     D  TOTD_VOL                      9  2
008700061215     D*
008800061215     D DS_WST94_TOTG   DS
008900061215     D  TOTG_NFV                      7  0
009000061215     D  TOTG_CGI                     10
009100061215     D  TOTG_BOL                      9  0
009200061219     D  TOTG_NCL                      9  0
009300061221     D  TOTG_PES                      9  0
009400061221     D  TOTG_VOL                      9  2
009500120403     D*
009600120403     D DS_WST94_TGM    DS
009700120403     D  TGm_NFV                       7  0
009800120403     D  TGm_CGI                      10
009900120403     D  TGm_BOL                       9  0
010000120403     D  TGm_NCL                       9  0
010100120403     D  TGm_PES                       9  0
010200120403     D  TGm_VOL                       9  2
010300061215     D*------------------
010400061215     D* DS "XSRDA8" - CONTROLLA DATA (8)
010500061215     D*------------------
010600061215     D WLBDA8          DS
010700061215     D  G08DAT                 1      8  0
010800061215     D  G08INV                 9     16  0
010900061215     D  G08ERR                17     17
011000061215     D  G08TGI                18     22  0
011100070202     D*------------------
011200070202     D* SCHIERE D WRK
011300070202     D*------------------
011400070202     D cchco           S              2    DIM(30)
011500070202     D REC             S              2    DIM(50)
011600070202     D*------------------
011700070202     D* DS TABELLE
011800070202     D*------------------
011900070202     D DCCH          E DS
012000070202     D DS3A          E DS
012100070202     D*------------------
012200070202     D* DS controllo della presenza di CA per la spedizione richiesta
012300070202     D*------------------
012400070202     D FIDN12DS      e ds
012500070202     D  skKey                 26    305    dim(20)
012600070202     D  skAnn                306    325    dim(20)
012700070202     D  skDca                326    485  0 dim(20)
012800070202     D  skFca                486    545  0 dim(20)
012900070202     D  skDch                546    705  0 dim(20)
013000070202     D  skCch                706    745    dim(20)
013100070202     D*
013200070202     D dsKey           ds
013300070202     D  dsaac                         4  0
013400070202     D  dsfil                         3  0
013500070202     D  dsnca                         7  0
013600061215     D*------------------
013700061215     D* VARIABILI D WRK
013800061215     D*------------------
013900061215     D FGS_DA          S              3  0 inz(*loval)
014000061215     D FGS_A           S              3  0 inz(*hival)
014100061215     D DFV_DA          S              8  0 inz(*loval)
014200061215     D DFV_A           S              8  0 inz(*hival)
014300061215     D NFV_DA          S              7  0 inz(*loval)
014400061215     D NFV_A           S              7  0 inz(*hival)
014500061222     D SAV_NFV         S                   like(DET_NFV) inz
014600061222     D SAV_CGI         S                   like(DET_CGI) inz
014700070215     D SAV_PTY         S                   like(DET_PTY) inz
014800061222     D In_DFV_stp      S                   like(In_DFV)  inz
014900070226     D In_PIG_num      S              1  0 inz
015000070117     D wTotBolle       S              7  0 inz(*zeros)
015100120403     D wTotBollem      S              7  0 inz(*zeros)
015200070202     D wTBEcod         S                   like(tbecod)
015300070202     D KTBkut          S                   like(tblkut) inz(1)
015400070202     D KTBcod          S                   like(tblcod)
015500061215     C*--------------------------------------------------------------------------------------------*
015600061215     C* Main lines
015700061215     C*--------------------------------------------------------------------------------------------*
015800071019     C*
015900071019     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
016000071019     C
016100071019     C/EXEC SQL
016200071019     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016300071019     C/END-EXEC
016400061215     C*
016500061215     C* Reperimento dati utente
016600061215     C                   exsr      repdatiute
016700070202     C*
016800070202     C* Caricamento P.O. in £6
016900070202     C                   exsr      carlira
017000070202     C*
017100070202     C* Caricamento dati tabellati
017200070202     C                   exsr      cartab
017300061215     C*
017400061215     C* Verifica parametri d lancio
017500061215     C                   exsr      chkpar
017600061215     C*
017700061215     C* Deleto la tabella temporanea d wrk
017800061215     C                   exsr      dlttab
017900061215     C*
018000061215     C* Creo la tabella temporanea d wrk
018100061215     C                   exsr      crttab
018200061215     C*
018300061214     C* Reperisco i dati e popolo la tabella temporanea
018400070205     C                   if        In_DFV   = *zeros AND
018500070205     C                             In_NFV   = *zeros
018600070205     C                   exsr      repdatinodfv
018700070205     C                   else
018800070205     C                   exsr      repdatisidfv
018900070205     C                   endif
019000070202     C*
019100070202     C* Se lancio x bolle nn in distinta => eseguo scrematura bolle
019200070202     C                   exsr      exclboll
019300061214     C*
019400061220     C* A seconda del tipo lancio eseguo:
019500061214     C* - distinta/giro
019600070215     C                   if        In_TIP  = 'DG' and
019700070215     C                             In_LIV <> 'T'
019800061214     C                   exsr      sta_ndc_cgi
019900061220     C                   endif
020000070215     C* - distinta/priorità
020100070215     C                   if        In_TIP = 'DG' and
020200070215     C                             In_LIV = 'T'
020300070215     C                   exsr      sta_ndc_pty
020400061220     C                   endif
020500061220     C* - giro
020600061220     C                   if        In_TIP = 'G'
020700061220     C                   seton                                        52
020800061220     C                   exsr      sta_cgi
020900061220     C                   endif
021000061215     C*
021100061215     C* Termine programma
021200061215     C                   seton                                        lr
021300060515     C*
021400061215     C*------------------------------------------------------------------------*
021500061215     C* CHKPAR - Verifica parametri d lancio
021600061215     C*------------------------------------------------------------------------*
021700061215     C     CHKPAR        BEGSR
021800061215     C*
021900061215     C* P.O. gestione
022000070202     C****               eval      FGS_DA = In_FGS
022100070202     C****               eval      FGS_A  = In_FGS
022200061215     C*
022300061215     C* Data distinta
022400061215     C                   if        In_DFV <> *zeros
022500061215     C                   eval      DFV_DA = In_DFV
022600061215     C                   eval      DFV_A  = In_DFV
022700061222     C* - giro la data distinta
022800061222     C                   z-add     In_DFV        G08INV
022900061222     C                   z-add     *ZEROS        G08DAT
023000061222     C                   movel     '3'           G08ERR
023100061222     C                   call      'XSRDA8'
023200061222     C                   parm                    WLBDA8
023300061222     C                   z-add     G08DAT        In_DFV_stp
023400061215     C                   endif
023500061215     C*
023600061215     C* Numero distinta
023700061215     C                   if        In_NFV <> *zeros
023800061215     C                   eval      NFV_DA = In_NFV
023900061215     C                   eval      NFV_A  = In_NFV
024000061215     C                   endif
024100070202     C*
024200070202     C* Se lancio x bolle NN in distinta verifico consegnabilità d ogni bolla
024300070202     C                   if        In_DFV   = *zeros AND
024400070202     C                             In_NFV   = *zeros
024500070202     C                   eval      DFV_DA = *loval
024600070202     C                   eval      DFV_A  = *hival
024700070205     C                   eval      NFV_DA = *zeros
024800070205     C                   eval      NFV_A  = *zeros
024900070202     C                   endif
025000061215     C*
025100061215     C                   ENDSR
025200061215     C*------------------------------------------------------------------------*
025300061215     C* DLTTAB - Routine di cancellazione tabella temporanea
025400061215     C*------------------------------------------------------------------------*
025500061215     C     DLTTAB        BEGSR
025600061215     C*
025700061215     C/EXEC SQL
025800061215     C+ DROP TABLE QTEMP/WFST940F
025900061215     C/END-EXEC
026000061215     C*
026100061215     C                   ENDSR
026200061214     C*------------------------------------------------------------------------*
026300061214     C* CRTTAB - Routine di creazione tabella temporanea
026400061214     C*------------------------------------------------------------------------*
026500061214     C     CRTTAB        BEGSR
026600061214     C*
026700061214     C/EXEC SQL
026800061214     C+ CREATE TABLE QTEMP/WFST940F (
026900061214     C+ WST94FGS DEC (3, 0) NOT NULL WITH DEFAULT,
027000061214     C+ WST94NPG CHAR (1 ) NOT NULL WITH DEFAULT,
027100061214     C+ WST94NFV DEC (7, 0) NOT NULL WITH DEFAULT,
027200061214     C+ WST94DFV DEC (8, 0) NOT NULL WITH DEFAULT,
027300061214     C+ WST94FPP CHAR (1 ) NOT NULL WITH DEFAULT,
027400061214     C+ WST94PDR DEC (7, 0) NOT NULL WITH DEFAULT,
027500061214     C+ WST94TIP CHAR (1 ) NOT NULL WITH DEFAULT,
027600061214     C+ WST94RSC CHAR (35 ) NOT NULL WITH DEFAULT,
027700120404     C+ WST94FLR char (30 ) NOT NULL WITH DEFAULT,
027800061214     C+ WST94CGI CHAR (10 ) NOT NULL WITH DEFAULT,
027900070215     C+ WST94TGI CHAR (1 ) NOT NULL WITH DEFAULT,
028000070129     C+ WST94PTY DEC (2, 0) NOT NULL WITH DEFAULT,
028100061222     C+ WST94LNO CHAR (3 ) NOT NULL WITH DEFAULT,
028200061222     C+ WST94ERR CHAR (2 ) NOT NULL WITH DEFAULT,
028300061214     C+ WST94AAS DEC (4, 0) NOT NULL WITH DEFAULT,
028400061214     C+ WST94LNP DEC (3, 0) NOT NULL WITH DEFAULT,
028500061214     C+ WST94NRS DEC (2, 0) NOT NULL WITH DEFAULT,
028600061214     C+ WST94NSP DEC (7, 0) NOT NULL WITH DEFAULT,
028700070202     C+ WST94LNA DEC (3, 0) NOT NULL WITH DEFAULT,
028800061214     C+ WST94NCL DEC (5, 0) NOT NULL WITH DEFAULT,
028900061221     C+ WST94PES DEC (7, 1) NOT NULL WITH DEFAULT,
029000061221     C+ WST94VOL DEC (5, 3) NOT NULL WITH DEFAULT,
029100061220     C+ WST94RSD CHAR (35 ) NOT NULL WITH DEFAULT,
029200061220     C+ WST94IND CHAR (35 ) NOT NULL WITH DEFAULT,
029300061220     C+ WST94LOD CHAR (35 ) NOT NULL WITH DEFAULT,
029400061220     C+ WST94CAD CHAR (9 ) NOT NULL WITH DEFAULT,
029500061221     C+ WST94PRD CHAR (2 ) NOT NULL WITH DEFAULT,
029600120403     C+ WST94ZNC DEC (2, 0) NOT NULL WITH DEFAULT)
029700061214     C/END-EXEC
029800061214     C*
029900061214     C                   ENDSR
030000061214     C*------------------------------------------------------------------------*
030100070205     C* REPDATISIDFV - Routine di reperimento dati rispetto alle selezioni a video - si distinta
030200061214     C*------------------------------------------------------------------------*
030300070202     C     REPDATISIDFV  BEGSR
030400061214     C*
030500061214     C/EXEC SQL
030600070129     C+ insert into qtemp/wfst940f
030700120604     C+ select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp, dstpdr,
030800120403     C+ apdtip, apdrsc, apdflr, argcgi, argtgi,
030900120604     C+ getcgipty(apdtip, dstpdr, dstfpp, dstfgs, argcgi, dstdfv,
031000120604     C+ dstnfv, :In_PIG_num),
031100120604     C+ arglno, argerr, arbaas, arblnp, arbnrs, arbnsp, arblna,
031200061221     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
031300061221     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
031400120403     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
031500061214     C+ from fiarg00f join
031600120604     C+ (select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp, dstpdr, apdtip,
031700120604     C+ apdrsc, apdflr, arbaas, arblnp, arbnrs, arbnsp, arblna,
031800061221     C+ arbncl, arbncr, arbncp, arbpkf, arbpkc, arbfvf, arbvlf, arbvlc,
031900061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
032000061214     C+ from fnarb00f join
032100120604     C+ (select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp,
032200120403     C+ dstpdr, apdtip, apdrsc, apdflr
032300120604     C+ from fidst00f join
032400120604     C+ fiapd00f on dstpdr=apdpdr
032500120604     C+ where apdtip='A' and
032600120604     C+ dstnpg='4' and dstfcf=' '
032700120604     C+ and dstdfv >= :DFV_DA  and dstdfv <= :DFV_A
032800120604     C+ and dstnfv >= :NFV_DA  and dstnfv <= :NFV_A) as distinte on
032900120604     C+ dstfgs=arbifp and dstnfv=arbndc and dstdfv=arbddc) as bolle on
033000061214     C+ arbaas=argaas and arblnp=arglnp and
033100061214     C+ arbnrs=argnrs and arbnsp=argnsp
033200061214     C+ union
033300120604     C+ select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp, dstpdr,
033400120403     C+ apdtip, apdrsc, apdflr, argcgi, argtgi,
033500120604     C+ getcgipty(apdtip, dstpdr, dstfpp, dstfgs, argcgi, dstdfv,
033600120604     C+ dstnfv, :In_PIG_num),
033700070129     C+ arglno, argerr,
033800070202     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp, arblna,
033900061221     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
034000061221     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
034100061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
034200061221     C+ from fnarb00f join
034300120604     C+ (select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp, dstpdr,
034400120403     C+ apdtip, apdrsc, apdflr,
034500120403     C+ argcgi, argtgi, arglno, argerr,
034600070202     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp,
034700061214     C+ ftdncl, ftdpkl, ftdvlu
034800061214     C+ from fiarg00f join
034900120604     C+ (select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp, dstpdr, apdtip,
035000120403     C+ apdrsc, apdflr,
035100120403     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp, ftdncl, ftdpkl, ftdvlu
035200061214     C+ from fiftd00f join
035300120604     C+ (select dstfgs, dstnpg, dstnfv, dstdfv, dstfpp,
035400120403     C+ dstpdr, apdtip, apdrsc, apdflr
035500120604     C+ from fidst00f join
035600120604     C+ fiapd00f on dstpdr=apdpdr
035700120604     C+ where apdtip='A' and
035800120604     C+ dstnpg='4' and dstfcf<>' '
035900120604     C+ and dstdfv >= :DFV_DA  and dstdfv <= :DFV_A
036000120604     C+ and dstnfv >= :NFV_DA  and dstnfv <= :NFV_A) as distinte on
036100120604     C+ dstfgs=ftdfgs and dstdfv=ftdddc and dstnfv=ftdndc
036200061214     C+ where ftdtsr='C') as bolle on
036300061214     C+ ftdaas=argaas and ftdlnp=arglnp and
036400061220     C+ ftdnrs=argnrs and ftdnsp=argnsp) as estrazione on
036500061220     C+ ftdaas=arbaas and ftdlnp=arblnp and
036600061220     C+ ftdnrs=arbnrs and ftdnsp=arbnsp
036700061214     C/END-EXEC
036800061214     C*
036900061214     C                   ENDSR
037000070202     C*------------------------------------------------------------------------*
037100070205     C* REPDATINODFV - Routine di reperimento dati rispetto alle selezioni a video - no distinta
037200070202     C*------------------------------------------------------------------------*
037300070202     C     REPDATINODFV  BEGSR
037400070202     C*
037500070205     C/EXEC SQL
037600070205     C+ insert into qtemp/wfst940f
037700070608     C*+ select 0, ' ', 0, 0, ' ', 0, ' ', ' ', argcgi, argtgi, 0,
037800151116     C+ select :In_FGS, ' ', 0, 0, ' ', 0, ' ', ' ', ' ', argcgi, argtgi,
037900151116     C+ 0, arglno, argerr, arbaas, arblnp, arbnrs, arbnsp, arblna,
038000070205     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
038100070205     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
038200070205     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
038300070205     C+ from filtra201/fnarb00f join filtra201/fiarg00f
038400070205     C+ on arbaas=argaas and arblnp=arglnp and arbnrs=argnrs and
038500070205     C+ arbnsp=argnsp
038600070205     C+ where arbndc=0
038700070205     C/END-EXEC
038800070202     C*
038900070202     C                   ENDSR
039000070202     C*------------------------------------------------------------------------*
039100070202     C* EXCLBOLL - Routine di esclusione bolle
039200070202     C*------------------------------------------------------------------------*
039300070202     C     EXCLBOLL      BEGSR
039400070205     C*
039500070202     C/EXEC SQL
039600070202     C+ declare C9 cursor for
039700070202     C+ select * from wfst940f
039800070202     C/END-EXEC
039900070202     C
040000070202     C/EXEC SQL
040100070202     C+ open C9
040200070202     C/END-EXEC
040300070202     C
040400070202     C/EXEC SQL
040500070202     C+ Fetch next from C9 into :DS_WST94_DET
040600070202     C/END-EXEC
040700070202     C
040800070202     C*
040900070202     C* Se reperito qualcosa => analizzo
041000070202     C                   dow       sqlcod = *zeros
041100070205     C*
041200070205     C                   movel     'N'           Escbolla          1
041300070202     C*
041400070202     C* Escludo bolle nn in "lira" del P.O. richiesto
041500070202     C     DET_LNA       lookup    Lin                                    99
041600070202     C                   if        not *IN99 = *on
041700070202     C                   eval      Escbolla='S'
041800070202     C                   endif
041900070202     C*
042000070202     C* Se lancio x bolle NN in distinta verifico consegnabilità d ogni bolla
042100070202     C                   if        In_DFV   = *zeros AND
042200070202     C                             In_NFV   = *zeros AND
042300070202     C                             Escbolla = 'N'
042400070202     C*
042500070202     C* Aggancio la bolla
042600070202     C     KEYarb01_c    chain     fnarb01l
042700070202     C                   if        %found(fnarb01l)
042800070202     C*
042900070202     C* Verifico consegnabilità bolla
043000070202     C                   exsr      escludi
043100070202     C*
043200070202     C                   endif
043300070202     C                   endif
043400070202     C*
043500070202     C* Se bolla da escludere => elimino il record corrente dalla tabella sql
043600070202     C                   if        Escbolla = 'S'
043700070202     C/EXEC SQL
043800070202     C+ delete from wfst940f where CURRENT OF C9
043900070202     C/END-EXEC
044000070202     C                   endif
044100070202     C*
044200070202     C* Proseguo con la lettura della tabella sql
044300070202     C/EXEC SQL
044400070202     C+ Fetch next from C9 into :DS_WST94_DET
044500070202     C/END-EXEC
044600070202     C*
044700070202     C                   enddo
044800070202     C*
044900070202     C/EXEC SQL
045000070202     C+ close C9
045100070202     C/END-EXEC
045200070202     C*
045300070202     C                   ENDSR
045400060516     C*------------------------------------------------------------------------*
045500061214     C* STA_NDC_CGI - Routine di stampa x distita/giro
045600060516     C*------------------------------------------------------------------------*
045700061214     C     STA_NDC_CGI   BEGSR
045800061215     C*
045900061215     C* Apertura printer-file
046000061215     C                   open      TIST94P
046100061220     C*
046200061220     C* Valorizzo titolo tabulato
046300070125     C                   eval      stttes1 = '   *** VERIFICA ABBINAMENTI ' +
046400070117     C                                       'DISTINTE/GIRI ***'
046500070201     C                   if        In_LIV = 'S'
046600070125     C                   eval      stttes2 = '(SINTETICA)'
046700070117     C                   else
046800070201     C                   eval      stttes2 = '(DETTAGLIO)'
046900070201     C                   endif
047000061222     C*
047100061222     C* Valorizzo i paramteri d lancio
047200120404     C                   eval      stppar = 'Fil.: ' +
047300070201     C                                       %trim(%editw(In_FGS:'0   '))
047400061222     C                   if        In_NFV <> *zeros
047500070116     C                   eval      stppar = %trim(stppar) + ' '+
047600070116     C                                      'PARAMETRI: tutti le bolle nella ' +
047700061222     C                                      'distinta '                        +
047800061222     C                                       %trim(%editw(In_FGS:'0   '))      +
047900061222     C                                       ' '                               +
048000061222     C                                       %trim(%editc(In_NFV:'4'))
048100061222     C                   else
048200070201     C                   if        In_DFV <> *zeros
048300070116     C                   eval      stppar = %trim(stppar) + ' '+
048400070116     C                                      'PARAMETRI: tutti le bolle nelle ' +
048500061222     C                                      'distinte in data '                +
048600061222     C                                      %editw(In_DFV_stp:'0  /  /    ')
048700070201     C                   else
048800070201     C                   eval      stppar = %trim(stppar) + ' '+
048900070201     C                                      'PARAMETRI: tutti le bolle non ' +
049000070201     C                                      'inserite in nessuna distinta'
049100070201     C                   endif
049200061222     C                   endif
049300061218     C*
049400061218     C* Se scelta stampa nn dettaglio stampo subito la testata
049500061220     C                   if        *in40 = *off
049600061218     C                   write     st94te
049700061222     C                   seton                                        60
049800061220     C                   write     st94dte
049900061220     C                   write     st94ri
050000061222     C                   setoff                                       05
050100061218     C                   endif
050200060516     C*
050300060516     C/EXEC SQL
050400061215     C+ declare C1 cursor for
050500061214     C+ select * from wfst940f
050600070205     C+ order by wst94nfv, wst94pty, wst94cgi,
050700070205     C+ wst94prd, wst94lod, wst94ind, wst94cad, wst94rsd
050800061214     C+ for read only
050900060516     C/END-EXEC
051000060516     C
051100060516     C/EXEC SQL
051200061215     C+ open C1
051300060516     C/END-EXEC
051400060516     C
051500060516     C/EXEC SQL
051600061215     C+ Fetch next from C1 into :DS_WST94_DET
051700060516     C/END-EXEC
051800060516     C*
051900060516     C* Se reperito qualcosa => analizzo
052000061214     C                   dow       sqlcod = *zeros
052100061220     C*
052200061220     C* Valorizzo i dati in stampa relativi alla distinta
052300061220     C*
052400061220     C* - codice distinta
052500061220     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
052600061220     C                                       ' ' +
052700061220     C                                       %trim(%editc(det_nfv:'4'))
052800061220     C* - autotrasportatore
052900061220     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
053000061220     C                                       ' ' +
053100061220     C                                       det_rsc
053200120403     c                   if        %subst(det_flr: 9: 7) <> ' '
053300120404     c                   eval      det_mcp = %subst(det_flr: 9: 6)
053400120403     c                   move      det_mcp       stpmcp
053500120403     c                   else
053600120403     c                   clear                   stpmcp
053700120403     c                   end
053800061220     C* - data distinta
053900061220     C                   z-add     det_dfv       G08INV
054000061220     C                   z-add     *ZEROS        G08DAT
054100061220     C                   movel     '3'           G08ERR
054200061220     C                   call      'XSRDA8'
054300061220     C                   parm                    WLBDA8
054400061220     C                   z-add     G08DAT        stpdat
054500061220     C* - prestazione distinta
054600061220     C                   if        det_fpp = 'M'
054700061220     C                   eval      stpprest = 'MAT.'
054800061220     C                   endif
054900061220     C                   if        det_fpp = 'P'
055000061220     C                   eval      stpprest = 'POM.'
055100061220     C                   endif
055200061218     C*
055300061218     C* Se scelta stampa anche dettaglio ad ogni distinta salto pagina
055400061220     C                   if        *in40 = *on
055500061220     C                   eval      stpdes = 'Distinta: '+
055600061220     C                                %trim(stpdist)+' del '+
055700061220     C                                %editw(stpdat:'0  /  /    ')+' '+stpprest+
055800061220     C                                '    '+'Autotrasportatore: '+
055900061220     C                                stpaut
056000061222     C   60              write     st94ri
056100061222     C   60              write     st94er
056200061220     C                   write     st94te
056300061222     C                   seton                                        60
056400061222     C                   setoff                                       05
056500061218     C                   endif
056600061218     C*
056700061220     C* Inizializzo l'indicatore d nuova distinta
056800061218     C                   setoff                                       51
056900061215     C*
057000061215     C* Salvo la distinta corrente
057100061215     C                   eval      SAV_NFV = DET_NFV
057200061215     C*
057300061215     C/EXEC SQL
057400061215     C+ declare C2 cursor for
057500061219     C+ select wst94nfv, count(*), decimal(sum(wst94ncl), 9, 0),
057600061221     C+ decimal(sum(wst94pes), 9, 0), decimal(sum(wst94vol), 9, 2)
057700061219     C+ from wfst940f
057800061219     C+ where wst94nfv = :SAV_NFV
057900061221     C+ group by wst94nfv
058000061215     C+ for read only
058100061215     C/END-EXEC
058200061215     C
058300061215     C/EXEC SQL
058400061215     C+ open C2
058500061215     C/END-EXEC
058600061215     C
058700061215     C/EXEC SQL
058800061218     C+ Fetch next from C2 into :DS_WST94_TOTD
058900061215     C/END-EXEC
059000061215     C
059100061215     C/EXEC SQL
059200061215     C+ close C2
059300061215     C/END-EXEC
059400061215     C*
059500061215     C* Ad ogni rottura d distinta reperisco il totale bolle in distinta
059600061215     C                   dow       sqlcod = *zeros AND
059700061215     C                             DET_NFV = SAV_NFV
059800061215     C*
059900061215     C* Salvo il giro corrente
060000061215     C                   eval      SAV_CGI = DET_CGI
060100061215     C*
060200061215     C/EXEC SQL
060300061215     C+ declare C3 cursor for
060400070129     C+ select wst94nfv, wst94cgi, count(*),
060500070129     C+ decimal(sum(wst94ncl), 9, 0),
060600061221     C+ decimal(sum(wst94pes), 9, 0),
060700061221     C+ decimal(sum(wst94vol)+0.005, 9, 2)
060800061219     C+ from wfst940f
060900061215     C+ where wst94nfv = :SAV_NFV and wst94cgi = :SAV_CGI
061000061221     C+ group by wst94nfv, wst94cgi
061100061215     C+ for read only
061200061215     C/END-EXEC
061300061215     C
061400061215     C/EXEC SQL
061500061215     C+ open C3
061600061215     C/END-EXEC
061700061215     C
061800061215     C/EXEC SQL
061900061215     C+ Fetch next from C3 into :DS_WST94_TOTG
062000061215     C/END-EXEC
062100061215     C
062200061215     C/EXEC SQL
062300061215     C+ close C3
062400061215     C/END-EXEC
062500061220     C*
062600061220     C* Valorizzo i dati in stampa relativi al giro
062700070202     C                   clear                   stpcgipty
062800061215     C                   eval      stpgiro   = det_cgi
062900070202     C* - decodifica giro e priorità
063000070125     C                   if        det_cgi <> *blanks
063100070215     C                   eval      stpcgipty = %editc(det_pty:'4')
063200070125     C                   clear                   fidg09ds
063300070125     C                   eval      d09iop0 = '001'
063400070202     C                   eval      d09ifgs = det_fgs
063500070125     C                   eval      d09idat = datcor
063600070125     C                   eval      d09icgi = det_cgi
063700070709     C                   eval      d09itug = 'C'
063800070125     C                   movel(p)  fidg09ds      KPJBU
063900070125     C                   call      'FIDG09R'
064000070125     C                   parm                    KPJBA
064100070125     C                   movel(p)  KPJBU         fidg09ds
064200070125     C                   if        d09oerr = *blanks
064300120403     C                   eval      dgrgflo  = d09oflo
064400120403     C                   eval      stpfgm   = §grgfgm
064500120411     C                   eval      stpcsto  = §grgcsto
064600120403     C                   eval      stpain   = d09oain
064700070125     C                   eval      stpgirod = d09odes
064800070125     C                   else
064900120403     C                   eval      stpfgm   = *all'*'
065000120411     C                   eval      stpcsto  = *all'*'
065100120403     C                   eval      stpain   = *all'*'
065200070125     C                   eval      stpgirod = *all'*'
065300070125     C                   endif
065400070125     C*
065500070125     C                   else
065600070215     C                   eval      stpain    = *blanks
065700070215     C                   eval      stpgirod  = '*** non attribuito ***'
065800070215     C                   eval      stpcgipty = '--'
065900070125     C                   endif
066000070202     C*
066100070202     C                   exsr      valdet
066200061218     C*
066300061218     C* Gestisco salto pagina
066400061218     C                   if        *in05 = *on
066500061222     C   60              write     st94ri
066600061222     C   60              write     st94er
066700061218     C                   write     st94te
066800061222     C                   seton                                        60
066900061220     C  N40              write     st94dte
067000061220     C  N40              write     st94ri
067100061218     C                   setoff                                       05
067200061218     C*
067300061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
067400061220     C                   if        *in40 = *on
067500061220     C                   write     st94ri
067600061220     C                   write     st94dtd
067700061220     C                   write     st94ri
067800070202     C                   write     st94gtd
067900070202     C                   write     st94ri
068000061218     C                   endif
068100061218     C                   endif
068200061219     C*
068300061219     C* Se richiesta stampa in dettaglio lascio 1 riga vuota ad ogni cambio giro
068400061220     C                   if        *in40 = *on
068500061219     C   51              write     st94sp
068600061219     C                   endif
068700061218     C*
068800061218     C* Quindi stampo riga totali x distinta/giro
068900070202     C                   if        *in40 = *off
069000070202     C                   if        *in51 = *off
069100070202     C                   write     st94dt
069200070202     C                   seton                                        51
069300070202     C                   exsr      valdet
069400070202     C                   write     st94dtg
069500070202     C                   else
069600070202     C                   write     st94dtg
069700070202     C                   endif
069800070202     C                   endif
069900070202     C*
070000070202     C   40              write     st94ri
070100070202     C   40              write     st94dtd
070200070202     C   40              write     st94ri
070300070202     C   40              write     st94gtd
070400070202     C   40              write     st94ri
070500061218     C                   seton                                        51
070600061215     C*
070700061215     C* Ad ogni rottura d giro reperisco i totali bolle, colli. peso, volume x distinta/giro
070800061219     C                   dow       sqlcod = *zeros   AND
070900061219     C                             DET_NFV = SAV_NFV AND
071000061215     C                             DET_CGI = SAV_CGI
071100061215     C*
071200061215     C* Se scelta stampa anche dettaglio => provvedo
071300061220     C                   if        *in40 = *on
071400061222     C                   eval      stdsped = %editc(det_aas:'4') +
071500061220     C                                       ' ' +
071600070116     C                                       %trim(%editw(det_lnp:'0   ')) +
071700061220     C                                       ' ' +
071800061222     C                                       %editc(det_nrs:'4') +
071900061220     C                                       ' ' +
072000061222     C                                       %editc(det_nsp:'4')
072100070215     C                   if        det_tgi = 'M'
072200070215     C                   eval      stdtgi = 'M'
072300070215     C                   else
072400070215     C                   eval      stdtgi = *blanks
072500070215     C                   endif
072600061220     C                   eval      stdrsd = det_rsd
072700061220     C                   eval      stdind = det_ind
072800061220     C                   eval      stdloc = det_lod
072900061220     C                   eval      stdcap = det_cad
073000061221     C                   eval      stdprd = det_prd
073100061220     C***                eval      stdznc = det_znc
073200070125     C                   eval      stdncl =
073300070605     C                                       %subst(%editc(det_ncl:'2'):1:6)
073400070125     C                   eval      stdpkf =
073500061221     C                                       %subst(%editc(det_pes:'2'):3:7)
073600070125     C                   eval      stdvlf = %editc(det_vol:'2')
073700061222     C*
073800061222     C* Gestisco eventuale errore "TELLUS"
073900061222     C                   setoff                                       53
074000070102     C                   if        det_cgi =  *blanks AND
074100070102     C                             det_err <> '10'
074200061222     C                   seton                                        53
074300070102     C                   if        det_err = *blanks
074400070102     C                   eval      stderr = '(' + %subst(det_lno:1:2) + ')'
074500061222     C                   else
074600070102     C                   eval      stderr = '(' + det_err + ')'
074700061222     C                   endif
074800061222     C                   endif
074900061218     C*
075000061218     C* Gestisco salto pagina
075100061218     C                   if        *in05 = *on
075200061222     C   60              write     st94ri
075300061222     C   60              write     st94er
075400061218     C                   write     st94te
075500061222     C                   seton                                        60
075600061220     C  N40              write     st94dte
075700061220     C  N40              write     st94ri
075800061218     C                   setoff                                       05
075900061218     C*
076000061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
076100061220     C                   if        *in40 = *on
076200061220     C                   write     st94ri
076300061220     C                   write     st94dtd
076400061220     C                   write     st94ri
076500070202     C                   write     st94gtd
076600070202     C                   write     st94ri
076700061218     C                   endif
076800061218     C                   endif
076900061215     C*
077000061215     C* Stampo riga d dettaglio bolle
077100061215     C                   write     st94dd
077200061215     C                   endif
077300060516     C*
077400061214     C* Proseguo con la lettura del recordset
077500061214     C/EXEC SQL
077600061215     C+ Fetch next from C1 into :DS_WST94_DET
077700061214     C/END-EXEC
077800061214     C                   enddo
077900061215     C*
078000061215     C                   enddo
078100070117     C*
078200070117     C* Totalizzo le bolle
078300070117     C                   eval      wTotBolle = wTotBolle + totd_bol
078400061215     C*
078500061215     C                   enddo
078600060516     C*
078700060516     C/EXEC SQL
078800061215     C+ close C1
078900060516     C/END-EXEC
079000070117     C*
079100070117     C                   evalr     stototbol = %editc(wTotBolle:'2')
079200061215     C*
079300061215     C* Chiusura printer-file
079400061220     C                   write     st94ri
079500070117     C                   write     st94tt
079600070117     C                   write     st94ri
079700061222     C                   write     st94er
079800061220     C                   write     st94bk
079900061220     C                   write     st94bk
080000061215     C                   write     st94fi
080100061215     C                   close     TIST94P
080200060516     C*
080300060516     C                   ENDSR
080400070215     C*------------------------------------------------------------------------*
080500070215     C* STA_NDC_PTY - Routine di stampa x distita/priorità (solo totali)
080600070215     C*------------------------------------------------------------------------*
080700070215     C     STA_NDC_PTY   BEGSR
080800070215     C*
080900070215     C* Apertura printer-file
081000070215     C                   open      TIST94P
081100070215     C*
081200070215     C* Valorizzo titolo tabulato
081300070215     C                   eval      stttes1 = '*** VERIFICA ABBINAMENTI ' +
081400070215     C                                       'DISTINTE/PRIORITA ***'
081500070215     C                   eval      stttes2 = ' (TOTALI) '
081600070215     C*
081700070215     C* Valorizzo i paramteri d lancio
081800120404     C                   eval      stppar = 'Fil.: ' +
081900070215     C                                       %trim(%editw(In_FGS:'0   '))
082000070215     C                   if        In_NFV <> *zeros
082100070215     C                   eval      stppar = %trim(stppar) + ' '+
082200070215     C                                      'PARAMETRI: tutti le bolle nella ' +
082300070215     C                                      'distinta '                        +
082400070215     C                                       %trim(%editw(In_FGS:'0   '))      +
082500070215     C                                       ' '                               +
082600070215     C                                       %trim(%editc(In_NFV:'4'))
082700070215     C                   else
082800070215     C                   if        In_DFV <> *zeros
082900070215     C                   eval      stppar = %trim(stppar) + ' '+
083000070215     C                                      'PARAMETRI: tutti le bolle nelle ' +
083100070215     C                                      'distinte in data '                +
083200070215     C                                      %editw(In_DFV_stp:'0  /  /    ')
083300070215     C                   else
083400070215     C                   eval      stppar = %trim(stppar) + ' '+
083500070215     C                                      'PARAMETRI: tutti le bolle non ' +
083600070215     C                                      'inserite in nessuna distinta'
083700070215     C                   endif
083800070215     C                   endif
083900070215     C*
084000070215     C* Se scelta stampa nn dettaglio stampo subito la testata
084100070215     C                   if        *in40 = *off
084200070215     C                   write     st94te
084300070215     C                   seton                                        60
084400070215     C                   write     st94dte
084500070215     C                   write     st94ri
084600070215     C                   setoff                                       05
084700070215     C                   endif
084800070215     C*
084900070215     C/EXEC SQL
085000070215     C+ declare C10 cursor for
085100070215     C+ select * from wfst940f
085200070215     C+ order by wst94nfv, wst94pty
085300070215     C+ for read only
085400070215     C/END-EXEC
085500070215     C
085600070215     C/EXEC SQL
085700070215     C+ open C10
085800070215     C/END-EXEC
085900070215     C
086000070215     C/EXEC SQL
086100070215     C+ Fetch next from C10 into :DS_WST94_DET
086200070215     C/END-EXEC
086300070215     C*
086400070215     C* Se reperito qualcosa => analizzo
086500070215     C                   dow       sqlcod = *zeros
086600070215     C*
086700070215     C* Valorizzo i dati in stampa relativi alla distinta
086800070215     C*
086900070215     C* - codice distinta
087000070215     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
087100070215     C                                       ' ' +
087200070215     C                                       %trim(%editc(det_nfv:'4'))
087300070215     C* - autotrasportatore
087400070215     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
087500070215     C                                       ' ' +
087600070215     C                                       det_rsc
087700120403     c                   if        %subst(det_flr: 9: 7) <> ' '
087800120404     c                   eval      det_mcp = %subst(det_flr: 9: 6)
087900120403     c                   move      det_mcp       stpmcp
088000120403     c                   else
088100120403     c                   clear                   stpmcp
088200120403     c                   end
088300070215     C* - data distinta
088400070215     C                   z-add     det_dfv       G08INV
088500070215     C                   z-add     *ZEROS        G08DAT
088600070215     C                   movel     '3'           G08ERR
088700070215     C                   call      'XSRDA8'
088800070215     C                   parm                    WLBDA8
088900070215     C                   z-add     G08DAT        stpdat
089000070215     C* - prestazione distinta
089100070215     C                   if        det_fpp = 'M'
089200070215     C                   eval      stpprest = 'MAT.'
089300070215     C                   endif
089400070215     C                   if        det_fpp = 'P'
089500070215     C                   eval      stpprest = 'POM.'
089600070215     C                   endif
089700070215     C*
089800070215     C* Inizializzo l'indicatore d nuova distinta
089900070215     C                   setoff                                       51
090000070215     C*
090100070215     C* Salvo la distinta corrente
090200070215     C                   eval      SAV_NFV = DET_NFV
090300070215     C*
090400070215     C/EXEC SQL
090500070215     C+ declare C11 cursor for
090600070215     C+ select wst94nfv, count(*),
090700070215     C+ decimal(sum(wst94ncl), 9, 0),
090800070215     C+ decimal(sum(wst94pes), 9, 0),
090900070215     C+ decimal(sum(wst94vol), 9, 2)
091000070215     C+ from wfst940f
091100070215     C+ where wst94nfv = :SAV_NFV
091200070215     C+ group by wst94nfv
091300070215     C+ for read only
091400070215     C/END-EXEC
091500070215     C
091600070215     C/EXEC SQL
091700070215     C+ open C11
091800070215     C/END-EXEC
091900070215     C
092000070215     C/EXEC SQL
092100070215     C+ Fetch next from C11 into :DS_WST94_TOTD
092200070215     C/END-EXEC
092300070215     C
092400070215     C/EXEC SQL
092500070215     C+ close C11
092600070215     C/END-EXEC
092700070215     C*
092800070215     C* Ad ogni rottura d distinta reperisco il totale bolle in distinta
092900070215     C                   dow       sqlcod = *zeros AND
093000070215     C                             DET_NFV = SAV_NFV
093100070215     C*
093200070215     C* Salvo il giro corrente
093300070215     C                   eval      SAV_PTY = DET_PTY
093400070215     C*
093500070215     C/EXEC SQL
093600070215     C+ declare C12 cursor for
093700070215     C+ select wst94nfv, wst94pty, count(*),
093800070215     C+ decimal(sum(wst94ncl), 9, 0),
093900070215     C+ decimal(sum(wst94pes), 9, 0),
094000070215     C+ decimal(sum(wst94vol)+0.005, 9, 2)
094100070215     C+ from wfst940f
094200070215     C+ where wst94nfv = :SAV_NFV and wst94pty = :SAV_PTY
094300070215     C+ group by wst94nfv, wst94pty
094400070215     C+ for read only
094500070215     C/END-EXEC
094600070215     C
094700070215     C/EXEC SQL
094800070215     C+ open C12
094900070215     C/END-EXEC
095000070215     C
095100070215     C/EXEC SQL
095200070215     C+ Fetch next from C12 into :DS_WST94_TOTG
095300070215     C/END-EXEC
095400070215     C
095500070215     C/EXEC SQL
095600070215     C+ close C12
095700070215     C/END-EXEC
095800070215     C*
095900070215     C* Valorizzo i dati in stampa relativi al giro
096000070215     C                   clear                   stpcgipty
096100070215     C                   eval      stpgiro   = *blanks
096200070215     C* - decodifica giro e priorità
096300070215     C                   if        det_cgi <> *blanks
096400070215     C                   eval      stpcgipty = %editc(det_pty:'4')
096500070215     C                   else
096600070215     C                   eval      stpcgipty = '--'
096700070215     C                   endif
096800070215     C*
096900070215     C                   exsr      valdet
097000070215     C*
097100070215     C* Gestisco salto pagina
097200070215     C                   if        *in05 = *on
097300070215     C   60              write     st94ri
097400070215     C   60              write     st94er
097500070215     C                   write     st94te
097600070215     C                   seton                                        60
097700070215     C  N40              write     st94dte
097800070215     C  N40              write     st94ri
097900070215     C                   setoff                                       05
098000070215     C                   endif
098100070215     C*
098200070215     C* Quindi stampo riga totali x distinta/priorità
098300070215     C                   if        *in51 = *off
098400070215     C                   write     st94dt
098500070215     C                   seton                                        51
098600070215     C                   exsr      valdet
098700070215     C                   write     st94dtg
098800070215     C                   else
098900070215     C                   write     st94dtg
099000070215     C                   endif
099100070215     C                   seton                                        51
099200070215     C*
099300070215     C* Ad ogni rottura d giro reperisco i totali bolle, colli. peso, volume x distinta/giro
099400070215     C                   dow       sqlcod = *zeros   AND
099500070215     C                             DET_NFV = SAV_NFV AND
099600070215     C                             DET_PTY = SAV_PTY
099700070215     C*
099800070215     C* Proseguo con la lettura del recordset
099900070215     C/EXEC SQL
100000070215     C+ Fetch next from C10 into :DS_WST94_DET
100100070215     C/END-EXEC
100200070215     C                   enddo
100300070215     C*
100400070215     C                   enddo
100500070215     C*
100600070215     C* Totalizzo le bolle
100700070215     C                   eval      wTotBolle = wTotBolle + totd_bol
100800070215     C*
100900070215     C                   enddo
101000070215     C*
101100070215     C/EXEC SQL
101200070215     C+ close C10
101300070215     C/END-EXEC
101400070215     C*
101500070215     C                   evalr     stototbol = %editc(wTotBolle:'2')
101600070215     C*
101700070215     C* Chiusura printer-file
101800070215     C                   write     st94ri
101900070215     C                   write     st94tt
102000070215     C                   write     st94ri
102100070215     C                   write     st94er
102200070215     C                   write     st94bk
102300070215     C                   write     st94bk
102400070215     C                   write     st94fi
102500070215     C                   close     TIST94P
102600070215     C*
102700070215     C                   ENDSR
102800070202     C*------------------------------------------------------------------------*
102900070202     C* VALDET - Routine valorizzazione dettaglio in stampa
103000070202     C*------------------------------------------------------------------------*
103100070202     C     VALDET        BEGSR
103200070202     C*
103300070202     C* Se sintetica....
103400070202     C                   if        *in40 = *off
103500070202     C   51              evalr     stptotbol =
103600070605     C                             %subst(%editc(totg_bol:'1'):7:5)
103700070202     C   51              eval      stpperc   = totg_bol * 100 / totd_bol
103800070202     C   51              evalr     stptotncl =
103900070202     C                             %subst(%editc(totg_ncl:'1'):7:5)
104000070202     C   51              evalr     stptotpkf =
104100070202     C                             %subst(%editc(totg_pes:'1'):6:6)
104200070202     C   51              evalr     stptotvlf =
104300070202     C                             %subst(%editc(totg_vol:'1'):8:5)
104400070202     C*
104500070202     C  N51              evalr     stptotbol =
104600070605     C                             %subst(%editc(totd_bol:'1'):7:5)
104700070202     C  N51              eval      stpperc   = *zeros
104800070202     C  N51              evalr     stptotncl =
104900070202     C                             %subst(%editc(totd_ncl:'1'):7:5)
105000070202     C  N51              evalr     stptotpkf =
105100070202     C                             %subst(%editc(totd_pes:'1'):6:6)
105200070202     C  N51              evalr     stptotvlf =
105300070202     C                             %subst(%editc(totd_vol:'1'):8:5)
105400070202     C                   else
105500070202     C                   evalr     stptotbol =
105600070605     C                             %subst(%editc(totg_bol:'1'):7:5) + '/' +
105700070605     C                             %subst(%editc(totd_bol:'1'):7:5)
105800070202     C                   eval      stpperc   = totg_bol * 100 / totd_bol
105900070202     C                   evalr     stptotncl =
106000070202     C                             %subst(%editc(totg_ncl:'1'):7:5) + '/' +
106100070202     C                             %subst(%editc(totd_ncl:'1'):7:5)
106200070202     C                   evalr     stptotpkf =
106300070202     C                             %subst(%editc(totg_pes:'1'):6:6) + '/' +
106400070202     C                             %subst(%editc(totd_pes:'1'):6:6)
106500070202     C                   evalr     stptotvlf =
106600070202     C                             %subst(%editc(totg_vol:'1'):8:5) + '/' +
106700070202     C                             %subst(%editc(totd_vol:'1'):8:5)
106800070202     C                   endif
106900070202     C*
107000070202     C                   ENDSR
107100061220     C*------------------------------------------------------------------------*
107200061220     C* STA_CGI - Routine di stampa x giro
107300061220     C*------------------------------------------------------------------------*
107400061220     C     STA_CGI       BEGSR
107500061220     C*
107600061220     C* Apertura printer-file
107700061220     C                   open      TIST94P
107800061220     C*
107900061220     C* Valorizzo titolo tabulato
108000070125     C                   eval      stttes1 = '      *** VERIFICA ASSORBIMENTO '+
108100070117     C                                       'GIRI ***'
108200070201     C                   if        In_LIV = 'S'
108300070201     C                   eval      stttes2 = '(SINTETICA)'
108400070201     C                   else
108500070201     C                   eval      stttes2 = '(DETTAGLIO)'
108600070201     C                   endif
108700061222     C*
108800061222     C* Valorizzo i paramteri d lancio
108900120404     C                   eval      stppar = 'Fil.: ' +
109000070116     C                                       %trim(%editw(In_FGS:'0   '))
109100070201     C                   if        In_DFV <> *zeros
109200070201     C                   eval      stppar = %trim(stppar) + ' '+
109300070201     C                                      'PARAMETRI: tutti le bolle nelle ' +
109400070201     C                                      'distinte in data '                +
109500070201     C                                      %editw(In_DFV_stp:'0  /  /    ')
109600070201     C                   else
109700070201     C                   eval      stppar = %trim(stppar) + ' '+
109800070201     C                                      'PARAMETRI: tutti le bolle non ' +
109900070201     C                                      'inserite in nessuna distinta'
110000070201     C                   endif
110100061220     C*
110200061220     C* Se scelta stampa nn dettaglio stampo subito la testata
110300061220     C                   if        *in40 = *off
110400061220     C                   write     st94te
110500061222     C                   seton                                        60
110600061220     C                   write     st94gte
110700061220     C                   write     st94ri
110800061222     C                   setoff                                       05
110900061220     C                   endif
111000061220     C*
111100061220     C/EXEC SQL
111200061220     C+ declare C7 cursor for
111300061220     C+ select * from wfst940f
111400061221     C+ order by wst94cgi, wst94prd, wst94lod,
111500061221     C+ wst94ind, wst94cad, wst94rsd
111600061220     C+ for read only
111700061220     C/END-EXEC
111800061220     C
111900061220     C/EXEC SQL
112000061220     C+ open C7
112100061220     C/END-EXEC
112200061220     C
112300070202     C/EXEC SQL
112400070202     C+ Fetch next from C7 into :DS_WST94_DET
112500070202     C/END-EXEC
112600070202     C
112700061220     C*
112800061220     C* Se reperito qualcosa => analizzo
112900070202     C                   dow       sqlcod = *zeros
113000061220     C*
113100061220     C* Inizializzo l'indicatore d nuovo giro
113200061220     C                   setoff                                       51
113300061220     C*
113400061220     C* Se scelta stampa anche dettaglio ad ogni giro salto pagina
113500061220     C                   if        *in40 = *on
113600061222     C   60              write     st94ri
113700061222     C   60              write     st94er
113800061220     C                   write     st94te
113900061222     C                   seton                                        60
114000061222     C                   setoff                                       05
114100061220     C                   endif
114200061220     C*
114300061220     C* Salvo il giro corrente
114400061220     C                   eval      SAV_CGI = DET_CGI
114500061220     C*
114600061220     C/EXEC SQL
114700061220     C+ declare C8 cursor for
114800061220     C+ select min(wst94nfv), wst94cgi, count(*),
114900061221     C+ decimal(sum(wst94ncl), 9, 0), decimal(sum(wst94pes), 9, 0),
115000061221     C+ decimal(sum(wst94vol), 9, 2) from wfst940f
115100061220     C+ where wst94cgi = :SAV_CGI
115200061221     C+ group by wst94cgi
115300061220     C+ for read only
115400061220     C/END-EXEC
115500061220     C
115600061220     C/EXEC SQL
115700061220     C+ open C8
115800061220     C/END-EXEC
115900061220     C
116000070202     C
116100070202     C*
116200061220     C/EXEC SQL
116300061220     C+ Fetch next from C8 into :DS_WST94_TOTG
116400061220     C/END-EXEC
116500061220     C
116600061220     C/EXEC SQL
116700061220     C+ close C8
116800061220     C/END-EXEC
116900120403     c* tottalizzo le spedizioni micro > 5 kg e 0,023 volume
117000120404     c                   clear                   sqlcod
117100120404     c                   clear                   DS_WST94_TGM
117200120404     c                   if        not *in40
117300120403     C/EXEC SQL
117400120403     C+ declare C4 cursor for
117500120404     C+ select min(wst94nfv), wst94cgi, count(*),
117600120404     C+ decimal(sum(wst94ncl), 9, 0), decimal(sum(wst94pes), 9, 0),
117700120404     C+ decimal(sum(wst94vol), 9, 2) from wfst940f
117800120404     C+ where wst94cgi = :SAV_CGI
117900120404     C+ and (wst94pes <= :§LIMPES  or wst94vol <= :§LIMvol )
118000120404     C+ group by wst94cgi
118100120403     C+ for read only
118200120403     C/END-EXEC
118300120403     C
118400120403     C/EXEC SQL
118500120403     C+ open C4
118600120403     C/END-EXEC
118700120403     C
118800120403     C/EXEC SQL
118900120403     C+ Fetch next from C4 into :DS_WST94_TGm
119000120403     C/END-EXEC
119100120403     C
119200120403     C/EXEC SQL
119300120403     C+ close C4
119400120403     C/END-EXEC
119500120404     c                   end
119600120404     c                   clear                   sqlcod
119700061220     C*
119800061220     C* Valorizzo i dati in stampa relativi al giro
119900061220     C*
120000061220     C* - codice giro
120100061220     C                   eval      stpgiro   = det_cgi
120200070125     C* - decodifica giro
120300070125     C                   if        det_cgi <> *blanks
120400070125     C                   clear                   fidg09ds
120500070125     C                   eval      d09iop0 = '001'
120600070202     C                   eval      d09ifgs = det_fgs
120700070125     C                   eval      d09idat = datcor
120800070125     C                   eval      d09icgi = det_cgi
120900070709     C                   eval      d09itug = 'C'
121000070125     C                   movel(p)  fidg09ds      KPJBU
121100070125     C                   call      'FIDG09R'
121200070125     C                   parm                    KPJBA
121300070125     C                   movel(p)  KPJBU         fidg09ds
121400070125     C                   if        d09oerr = *blanks
121500120403     C                   eval      dgrgflo  = d09oflo
121600120403     C                   eval      stpfgm   = §grgfgm
121700120411     C                   eval      stpcsto  = §grgcsto
121800070205     C                   eval      stpain   = d09oain
121900070205     C                   eval      stpgirod = d09odes
122000070125     C                   else
122100120403     C                   eval      stpfgm   = *all'*'
122200120411     C                   eval      stpcsto  = *all'*'
122300120403     C                   eval      stpain   = *all'*'
122400070205     C                   eval      stpgirod = *all'*'
122500070125     C                   endif
122600070125     C*
122700070125     C                   else
122800070205     C                   eval      stpain   = *blanks
122900070125     C                   eval      stpgirod = '*** non attribuito ***'
123000070125     C                   endif
123100120404     C* - totali
123200120404     C                   evalr     stptotbol =
123300120404     C                             %subst(%editc(totg_bol:'1'):7:5)
123400120404     C                   evalr     stptotncl =
123500120404     C                             %subst(%editc(totg_ncl:'1'):7:5)
123600120404     C                   evalr     stptotpkf =
123700120404     C                             %subst(%editc(totg_pes:'1'):6:6)
123800120404     C                   evalr     stptotvlf =
123900120404     C                             %subst(%editc(totg_vol:'1'):8:5)
124000120404     C                   evalr     stptotbolm=
124100120404     C                             %subst(%editc(tgm_bol:'2'):7:5)
124200120404     C                   evalr     stptotnclm=
124300120404     C                             %subst(%editc(tgm_ncl:'2'):7:5)
124400120404     C                   evalr     stptotpkfm=
124500120404     C                             %subst(%editc(tgm_pes:'2'):6:6)
124600120404     C                   evalr     stptotvlfm=
124700120404     C                             %subst(%editc(tgm_vol:'2'):8:5)
124800061220     C*
124900061220     C* Gestisco salto pagina
125000061220     C                   if        *in05 = *on
125100061222     C   60              write     st94ri
125200061222     C   60              write     st94er
125300061220     C                   write     st94te
125400061222     C                   seton                                        60
125500061220     C  N40              write     st94gte
125600070125     C   40              write     st94gtt
125700070125     C   40              write     st94ri
125800070125     C   40              write     st94gtd
125900061220     C                   write     st94ri
126000061220     C                   setoff                                       05
126100061220     C                   endif
126200061220     C*
126300061220     C* Stampo il giro corrente
126400070125     C   40              write     st94gtt
126500070125     C   40              write     st94ri
126600070125     C   40              write     st94gtd
126700061220     C   40              write     st94ri
126800061220     C  N40              write     st94gt
126900061220     C  N40              setoff                                       51
127000061220     C*
127100061220     C* Ad ogni rottura d giro se richiesto stampa il dettaglio bolle
127200061220     C                   dow       sqlcod = *zeros AND
127300061220     C                             DET_CGI = SAV_CGI
127400061220     C*
127500061220     C* Gestisco salto pagina
127600061220     C                   if        *in05 = *on
127700061222     C   60              write     st94ri
127800061222     C   60              write     st94er
127900061220     C                   write     st94te
128000061222     C                   seton                                        60
128100061220     C  N40              write     st94gte
128200070125     C   40              write     st94gtt
128300070125     C   40              write     st94ri
128400070125     C   40              write     st94gtd
128500061220     C                   write     st94ri
128600061220     C                   setoff                                       05
128700061220     C                   endif
128800061220     C*
128900061220     C* Se scelta stampa anche dettaglio => provvedo
129000061220     C                   if        *in40 = *on
129100061222     C                   eval      stdsped = %editc(det_aas:'4') +
129200061220     C                                       ' ' +
129300070116     C                                       %trim(%editw(det_lnp:'0   ')) +
129400061220     C                                       ' ' +
129500061222     C                                       %editc(det_nrs:'4') +
129600061220     C                                       ' ' +
129700061222     C                                       %editc(det_nsp:'4')
129800070215     C                   if        det_tgi = 'M'
129900070215     C                   eval      stdtgi = 'M'
130000070215     C                   else
130100070215     C                   eval      stdtgi = *blanks
130200070215     C                   endif
130300061220     C                   eval      stdrsd = det_rsd
130400061220     C                   eval      stdind = det_ind
130500061220     C                   eval      stdloc = det_lod
130600061220     C                   eval      stdcap = det_cad
130700061221     C                   eval      stdprd = det_prd
130800061220     C***                eval      stdznc = det_znc
130900070125     C                   eval      stdncl =
131000070605     C                                       %subst(%editc(det_ncl:'2'):1:6)
131100070125     C                   eval      stdpkf =
131200061221     C                                       %subst(%editc(det_pes:'2'):3:7)
131300070125     C                   eval      stdvlf = %editc(det_vol:'2')
131400070102     C*
131500070102     C* Gestisco eventuale errore "TELLUS"
131600070102     C                   setoff                                       53
131700070102     C                   if        det_cgi =  *blanks AND
131800070102     C                             det_err <> '10'
131900070102     C                   seton                                        53
132000070102     C                   if        det_err = *blanks
132100070102     C                   eval      stderr = '(' + %subst(det_lno:1:2) + ')'
132200070102     C                   else
132300070102     C                   eval      stderr = '(' + det_err + ')'
132400070102     C                   endif
132500070102     C                   endif
132600061220     C*
132700061220     C* Stampo riga d dettaglio bolle
132800061220     C                   write     st94dd
132900061220     C                   endif
133000070202     C/EXEC SQL
133100070202     C+ Fetch next from C7 into :DS_WST94_DET
133200070202     C/END-EXEC
133300070202     C*
133400061220     C                   enddo
133500070117     C*
133600070117     C* Totalizzo le bolle
133700070117     C                   eval      wTotBolle = wTotBolle + totg_bol
133800120403     C                   eval      wTotBollem= wTotBollem+ tgm_bol
133900061220     C*
134000061220     C                   enddo
134100061220     C*
134200061220     C/EXEC SQL
134300061220     C+ close C7
134400061220     C/END-EXEC
134500070117     C*
134600070117     C                   evalr     stototbol = %editc(wTotBolle:'2')
134700120403     C                   evalr     stototbolm= %editc(wTotBollem:'2')
134800070117     C*
134900070117     C* Chiusura printer-file
135000070117     C                   write     st94ri
135100070117     C                   write     st94tt
135200061220     C                   write     st94ri
135300061222     C                   write     st94er
135400061220     C                   write     st94bk
135500061220     C                   write     st94bk
135600061220     C                   write     st94fi
135700061220     C                   close     TIST94P
135800061220     C*
135900061220     C                   ENDSR
136000061215     C*--------------------------------------------------------------------------------------------*
136100061215     C* REPERISCE I DATI UTENTE
136200061215     C*--------------------------------------------------------------------------------------------*
136300061215     C     REPDATIUTE    BEGSR
136400061215     C*
136500061215     C* INIZIALIZZA VARIABILI DI WRK
136600061215     C                   CLEAR                   TIBS34DS
136700061215     C                   CLEAR                   AZUTEDS
136800061215     C                   CLEAR                   DDATIUTE
136900061215     C*
137000061215     C     *DTAARA       DEFINE    §azute        azuteds
137100061215     C     *DTAARA       DEFINE    §datiute      ddatiute
137200061215     C                   IN(E)     *DTAARA
137300061215     C                   IF        %Error
137400061215     c                   EVAL      I34Tla = 'L'
137500061215     C                   CALL      'TIBS34R'
137600061215     C                   PARM                    Tibs34Ds
137700061215     C                   IN        *DTAARA
137800061215     C                   ENDIF
137900061215     C*
138000061215     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
138100061215     C                   MOVEL(P)  RSUT          DSFIRS           20
138200061215     C*
138300061215     C                   ENDSR
138400070202     c*------------------------------------------------------------------
138500070202     c* Esclusione delle spedizioni da non passare a Tellus (presa da *pgm Elisa FNLRL1R)
138600070202     c*------------------------------------------------------------------
138700070202     c     ESCLUDI       BEGSR
138800070202     c*
138900070202     c                   clear                   Escbolla
139000070202     c*
139100070202     c* 1) Escludo bolle già consegnate
139200070202     c                   if        arbdcm > 0
139300070202     c                   eval      Escbolla='S'
139400070202     c                   goto      Endescludi
139500070202     c                   endif
139600070202     c*
139700070205     c* 2) Spedizioni già in distinta
139800070202     c* 3) Spedizioni in Fermo deposito
139900070202     c* 4) Spedizioni bloccate ("*" è per il cambio di porto)
140000070205     c                   if        (arbndc>0 and arbddc>0) or
140100070205     c                             arbffd<>' '             or
140200070205     c                             arbfbc='B' or arbfbc='*'
140300070202     c                   eval      Escbolla='S'
140400070202     c                   goto      Endescludi
140500070202     c                   endif
140600070202     c*
140700070202     c* 5) Tipo bolla di recupero
140800070202     c     arbcbo        lookup    REC                                    35
140900070202     c                   if        *in35
141000070202     c                   eval      Escbolla='S'
141100070202     c                   goto      Endescludi
141200070202     c                   endif
141300070202     c
141400070202     c* 6) con C.A. aperta
141500070202     c                   clear                   fidn12ds
141600070202     c                   eval      i12aas = arbaas
141700070202     c                   eval      i12lnp = arblnp
141800070202     c                   eval      i12nrs = arbnrs
141900070202     c                   eval      i12nsp = arbnsp
142000070202     c                   eval      i12fel = 'E'
142100070202     c                   eval      i12fan = 'E'
142200070202      *
142300070202     c                   call      'FIDN12R'
142400070202     c                   parm                    fidn12ds
142500070202      *
142600070202      * se non ci sono errori
142700070202     c                   if        o12err = ' '
142800070202      * se numero di CA trovate maggiore di zero
142900070202     c                   if        o12nca > 0
143000070202     c
143100070202     c                   z-add     *zeros        ii                2 0
143200070202     c     1             do        o12nca        ii
143300070202     c                   movea     skkey(ii)     dskey
143400070202     C                   Z-ADD     skdch(ii)     dctdch            8 0
143500070202     C                   movel     skcch(ii)     dctcch            2
143600070202      *
143700070202     C* -SE ESISTE C.A. APERTA  --> ESCO
143800070202     c                   If        dctdch = 0
143900070202     c                   eval      Escbolla='S'
144000070202     c                   leave
144100070202     c                   endif
144200070202     c
144300070202     c* Se c.a. chiusa con causale che non prevede consegna esco
144400070202     c                   if        dctcch<>*blanks
144500070202     c     dctcch        lookup    cchco                                  35
144600070202     c                   if        not *in35
144700070202     c                   eval      Escbolla='S'
144800070202     c                   leave
144900070202     c                   endif
145000070202     c                   endif
145100070202     c*
145200070202     C                   enddo
145300070202     c*
145400070202     c                   endif
145500070202     c                   endif
145600070202     C*
145700070202     C* -SE ESISTE C.A. APERTA  --> ESCO
145800070202    1C     Escbolla      IFEQ      'S'
145900070202     c                   goto      Endescludi
146000070202    1C                   ENDIF
146100070202     c*
146200070202     c* 7) Senza testare il record di giacenza, escludo tutte le bolle
146300070202     c*    con blocco G
146400070202    1c                   if        arbfbc='G'
146500070202     c                   eval      Escbolla='S'
146600070202     c                   goto      Endescludi
146700070202    3C                   endif
146800070202     C*
146900070202     c     Endescludi    ENDSR
147000070202     C*--------------------------------------------------------------------------------------------*
147100070202     C* CARTAB - Caricamento dati tabellati (logiche prese da *pgm Elisa FNLRL1R)
147200070202     C*--------------------------------------------------------------------------------------------*
147300070202     C     CARTAB        BEGSR
147400070202     C*
147500070202     c* Carico le causali chiusura c,a, che prevedono la consegna della merce
147600070202     c                   z-add     *zeros        xx                3 0
147700070202     C                   MOVEL     'CCH'         wTBEcod
147800070202     c     wTBEcod       chain     tntbe000                           31
147900070202      *
148000070202     c                   DOW       *in31 = *off
148100070202     c                   movel     TBEuni        DCCH
148200070202      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
148300070202     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
148400070202     c                             and TBEatb = *BLANKS  and  §cchccco= 'S'
148500070202     c                   add       1             xx
148600070202     c                   movel     TBEke1        cchco(XX)
148700070202     c                   endif
148800070202     c     wTBEcod       reade     tntbe000                               31
148900070202     c                   enddo
149000070202     c
149100070202     c* Carico tipi record di recupero
149200070202     C                   Z-ADD     1             KTBKUT
149300070202     C                   MOVE      '3A'          KTBCOD
149400070202     C                   CLEAR                   IY                4 0
149500070202     C     KEYtab_c      SETLL     TABEL00F
149600070202     C     KEYtab_c      READE     TABEL00F                               98
149700070202     C     *IN98         DOWEQ     *OFF
149800070202     C                   MOVEL     TBLUNI        DS3a
149900070202     C     §3arbl        IFEQ      'R'
150000070202     C                   ADD       1             IY
150100070202     C                   MOVEL     TBLKEY        rec(IY)
150200070202     C                   ENDIF
150300070202     C     KEYtab_c      READE     TABEL00F                               98
150400070202     C                   ENDDO
150500120403     c* valori spedizione micro
150600120404     c                   eval      tbecod='LIM'
150700120404     c                   eval      tbeke1='MICRO'
150800120403     C     KEYtbe        chain     Tntbe01l
150900120403     c                   if        %found(tntbe01l)
151000120403     c                   movel     tbeuni        dlimmicro
151100120403     c                   else
151200120403     c                   clear                   dlimmicro
151300120403     c                   end
151400070202     C*
151500070202     C                   ENDSR
151600070202     C*--------------------------------------------------------------------------------------------*
151700070202     C* CARICA I P.O. IN "lira" richiesta
151800070202     C*--------------------------------------------------------------------------------------------*
151900070202     C     CARLIRA       BEGSR
152000070202     C*
152100070202     C* CARICO TABELLA FILIALI GESTITE £6
152200070202     C                   clear                   trul06ds
152300070202     C                   move      '£6'          d06cod
152400070205     C                   movel     In_FGS        d06key
152500070202     C                   movel     trul06ds      kpjbu
152600070202     C                   call      'TRUL06R'
152700070202     C                   parm                    kpjba
152800070202     C                   movel     kpjbu         trul06ds
152900070202     C*
153000070202     C                   ENDSR
153100060515     C*--------------------------------------------------------------------------------------------*
153200060515     C* *inzsr - operazioni iniziali
153300060515     C*--------------------------------------------------------------------------------------------*
153400000000     C     *inzsr        BEGSR
153500060515     C*
153600060515     C* Ricevimento parametri
153700060515     C     *ENTRY        PLIST
153800061214     C                   PARM                    KPJBA
153900061214     C*
154000061214     C* Ridefinisco i parametri
154100061214     C                   MOVEL     KPJBU         PARAM
154200061220     C*
154300061220     C* Verifico subito se richiesta stampa in dettaglio o sintetica
154400070215     C                   setoff                                       40
154500070215     C                   setoff                                       54
154600070215     C                   if        In_LIV  = 'D'
154700070215     C                   seton                                        40
154800070215     C                   endif
154900070215     C                   if        In_LIV  = 'T'
155000070215     C                   seton                                        54
155100070215     C                   endif
155200070226     C                   if        In_PIG  = 'A'
155300070226     C                   eval      In_PIG_num = 1
155400070226     C                   else
155500070226     C                   eval      In_PIG_num = 2
155600070226     C                   endif
155700070125     C*
155800070125     C* CALCOLA LA DATA CORRENTE
155900070125     C                   time                    wn14             14 0
156000070125     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
156100070125     C                   z-add     wn8           g08dat
156200070125     C                   z-add     *zeros        g08inv
156300070125     C                   movel     '0'           g08err
156400070125     C                   call      'XSRDA8'
156500070125     C                   parm                    wlbda8
156600070125     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
156700070202     C*
156800070202     C* Definizione chiavi d lettura
156900070202     C* - FNARB01L
157000070202     C     KEYarb01_c    KLIST
157100070202     C                   KFLD                    DET_AAS
157200070202     C                   KFLD                    DET_LNP
157300070202     C                   KFLD                    DET_NRS
157400070202     C                   KFLD                    DET_NSP
157500070202     C* - TABEL00F
157600070202     C     KEYtab_c      KLIST
157700070202     C                   KFLD                    KTBKUT
157800070202     C                   KFLD                    KTBCOD
157900120403     C* - Tntbe01l
158000120403     C     KEYtbe        KLIST
158100120403     C                   KFLD                    tbecod
158200120403     C                   KFLD                    tbeke1
158300060515     C*
158400060515     C                   ENDSR
