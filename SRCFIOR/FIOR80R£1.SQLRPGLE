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
013900070202     D wTBEcod         S                   like(tbecod)
014000070202     D KTBkut          S                   like(tblkut) inz(1)
014100070202     D KTBcod          S                   like(tblcod)
014200061215     C*--------------------------------------------------------------------------------------------*
014300061215     C* Main lines
014400061215     C*--------------------------------------------------------------------------------------------*
014500061215     C*
014600061215     C* Reperimento dati utente
014700061215     C                   exsr      repdatiute
014800070202     C*
014900070202     C* Caricamento P.O. in £6
015000070202     C                   exsr      carlira
015100070202     C*
015200070202     C* Caricamento dati tabellati
015300070202     C                   exsr      cartab
015400061215     C*
015500061215     C* Verifica parametri d lancio
015600061215     C                   exsr      chkpar
015700061215     C*
015800061215     C* Deleto la tabella temporanea d wrk
015900061215     C                   exsr      dlttab
016000061215     C*
016100061215     C* Creo la tabella temporanea d wrk
016200061215     C                   exsr      crttab
016300061215     C*
016400061214     C* Reperisco i dati e popolo la tabella temporanea
016500070205     C                   if        In_DFV   = *zeros AND
016600070205     C                             In_NFV   = *zeros
016700070205     C                   exsr      repdatinodfv
016800070205     C                   else
016900070205     C                   exsr      repdatisidfv
017000070205     C                   endif
017100070202     C*
017200070202     C* Se lancio x bolle nn in distinta => eseguo scrematura bolle
017300070202     C                   exsr      exclboll
017400061214     C*
017500061220     C* A seconda del tipo lancio eseguo:
017600061214     C* - distinta/giro
017700070215     C                   if        In_TIP  = 'DG' and
017800070215     C                             In_LIV <> 'T'
017900061214     C                   exsr      sta_ndc_cgi
018000061220     C                   endif
018100070215     C* - distinta/priorità
018200070215     C                   if        In_TIP = 'DG' and
018300070215     C                             In_LIV = 'T'
018400070215     C                   exsr      sta_ndc_pty
018500061220     C                   endif
018600061220     C* - giro
018700061220     C                   if        In_TIP = 'G'
018800061220     C                   seton                                        52
018900061220     C                   exsr      sta_cgi
019000061220     C                   endif
019100061215     C*
019200061215     C* Termine programma
019300061215     C                   seton                                        lr
019400060515     C*
019500061215     C*------------------------------------------------------------------------*
019600061215     C* CHKPAR - Verifica parametri d lancio
019700061215     C*------------------------------------------------------------------------*
019800061215     C     CHKPAR        BEGSR
019900061215     C*
020000061215     C* P.O. gestione
020100070202     C****               eval      FGS_DA = In_FGS
020200070202     C****               eval      FGS_A  = In_FGS
020300061215     C*
020400061215     C* Data distinta
020500061215     C                   if        In_DFV <> *zeros
020600061215     C                   eval      DFV_DA = In_DFV
020700061215     C                   eval      DFV_A  = In_DFV
020800061222     C* - giro la data distinta
020900061222     C                   z-add     In_DFV        G08INV
021000061222     C                   z-add     *ZEROS        G08DAT
021100061222     C                   movel     '3'           G08ERR
021200061222     C                   call      'XSRDA8'
021300061222     C                   parm                    WLBDA8
021400061222     C                   z-add     G08DAT        In_DFV_stp
021500061215     C                   endif
021600061215     C*
021700061215     C* Numero distinta
021800061215     C                   if        In_NFV <> *zeros
021900061215     C                   eval      NFV_DA = In_NFV
022000061215     C                   eval      NFV_A  = In_NFV
022100061215     C                   endif
022200070202     C*
022300070202     C* Se lancio x bolle NN in distinta verifico consegnabilità d ogni bolla
022400070202     C                   if        In_DFV   = *zeros AND
022500070202     C                             In_NFV   = *zeros
022600070202     C                   eval      DFV_DA = *loval
022700070202     C                   eval      DFV_A  = *hival
022800070205     C                   eval      NFV_DA = *zeros
022900070205     C                   eval      NFV_A  = *zeros
023000070202     C                   endif
023100061215     C*
023200061215     C                   ENDSR
023300061215     C*------------------------------------------------------------------------*
023400061215     C* DLTTAB - Routine di cancellazione tabella temporanea
023500061215     C*------------------------------------------------------------------------*
023600061215     C     DLTTAB        BEGSR
023700061215     C*
023800061215     C/EXEC SQL
023900061215     C+ DROP TABLE QTEMP/WFST940F
024000061215     C/END-EXEC
024100061215     C*
024200061215     C                   ENDSR
024300061214     C*------------------------------------------------------------------------*
024400061214     C* CRTTAB - Routine di creazione tabella temporanea
024500061214     C*------------------------------------------------------------------------*
024600061214     C     CRTTAB        BEGSR
024700061214     C*
024800061214     C/EXEC SQL
024900061214     C+ CREATE TABLE QTEMP/WFST940F (
025000061214     C+ WST94FGS DEC (3, 0) NOT NULL WITH DEFAULT,
025100061214     C+ WST94NPG CHAR (1 ) NOT NULL WITH DEFAULT,
025200061214     C+ WST94NFV DEC (7, 0) NOT NULL WITH DEFAULT,
025300061214     C+ WST94DFV DEC (8, 0) NOT NULL WITH DEFAULT,
025400061214     C+ WST94FPP CHAR (1 ) NOT NULL WITH DEFAULT,
025500061214     C+ WST94PDR DEC (7, 0) NOT NULL WITH DEFAULT,
025600061214     C+ WST94TIP CHAR (1 ) NOT NULL WITH DEFAULT,
025700061214     C+ WST94RSC CHAR (35 ) NOT NULL WITH DEFAULT,
025800061214     C+ WST94CGI CHAR (10 ) NOT NULL WITH DEFAULT,
025900070215     C+ WST94TGI CHAR (1 ) NOT NULL WITH DEFAULT,
026000070129     C+ WST94PTY DEC (2, 0) NOT NULL WITH DEFAULT,
026100061222     C+ WST94LNO CHAR (3 ) NOT NULL WITH DEFAULT,
026200061222     C+ WST94ERR CHAR (2 ) NOT NULL WITH DEFAULT,
026300061214     C+ WST94AAS DEC (4, 0) NOT NULL WITH DEFAULT,
026400061214     C+ WST94LNP DEC (3, 0) NOT NULL WITH DEFAULT,
026500061214     C+ WST94NRS DEC (2, 0) NOT NULL WITH DEFAULT,
026600061214     C+ WST94NSP DEC (7, 0) NOT NULL WITH DEFAULT,
026700070202     C+ WST94LNA DEC (3, 0) NOT NULL WITH DEFAULT,
026800061214     C+ WST94NCL DEC (5, 0) NOT NULL WITH DEFAULT,
026900061221     C+ WST94PES DEC (7, 1) NOT NULL WITH DEFAULT,
027000061221     C+ WST94VOL DEC (5, 3) NOT NULL WITH DEFAULT,
027100061220     C+ WST94RSD CHAR (35 ) NOT NULL WITH DEFAULT,
027200061220     C+ WST94IND CHAR (35 ) NOT NULL WITH DEFAULT,
027300061220     C+ WST94LOD CHAR (35 ) NOT NULL WITH DEFAULT,
027400061220     C+ WST94CAD CHAR (9 ) NOT NULL WITH DEFAULT,
027500061221     C+ WST94PRD CHAR (2 ) NOT NULL WITH DEFAULT,
027600061220     C+ WST94ZNC DEC (2, 0) NOT NULL WITH DEFAULT)
027700061214     C/END-EXEC
027800061214     C*
027900061214     C                   ENDSR
028000061214     C*------------------------------------------------------------------------*
028100070205     C* REPDATISIDFV - Routine di reperimento dati rispetto alle selezioni a video - si distinta
028200061214     C*------------------------------------------------------------------------*
028300070202     C     REPDATISIDFV  BEGSR
028400061214     C*
028500061214     C/EXEC SQL
028600070129     C+ insert into qtemp/wfst940f
028700061220     C+ select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr,
028800070215     C+ apdtip, apdrsc, argcgi, argtgi,
028900070223     C+ getcgipty(apdtip, dstpdr, fvvfpp, fvvfgs, argcgi, fvvdfv,
029000070226     C+ fvvnfv, :In_PIG_num),
029100070129     C+ arglno, argerr,
029200070202     C+ arbaas, arblnp, arbnrs, arbnsp, arblna,
029300061221     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
029400061221     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
029500061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
029600061214     C+ from fiarg00f join
029700061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr, apdtip,
029800070202     C+ apdrsc, arbaas, arblnp, arbnrs, arbnsp, arblna,
029900061221     C+ arbncl, arbncr, arbncp, arbpkf, arbpkc, arbfvf, arbvlf, arbvlc,
030000061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
030100061214     C+ from fnarb00f join
030200061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp,
030300061214     C+ dstpdr, apdtip, apdrsc from fnfvv00f join
030400061214     C+ (select dstfgs, dstnfv, dstpdr, apdtip, apdrsc
030500061214     C+ from fndst00f join fiapd00f on dstpdr=apdpdr
030600070206     C+ where apdtip='A') as padroncini
030700061214     C+ on fvvfgs=dstfgs and fvvnfv=dstnfv where fvvnpg='4' and fvvfcf=' '
030800061214     C+ and fvvdfv >= :DFV_DA  and fvvdfv <= :DFV_A
030900061214     C+ and fvvnfv >= :NFV_DA  and fvvnfv <= :NFV_A) as distinte on
031000061214     C+ fvvfgs=arbifp and fvvnfv=arbndc and fvvdfv=arbddc) as bolle on
031100061214     C+ arbaas=argaas and arblnp=arglnp and
031200061214     C+ arbnrs=argnrs and arbnsp=argnsp
031300061214     C+ union
031400061220     C+ select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr,
031500070215     C+ apdtip, apdrsc, argcgi, argtgi,
031600070223     C+ getcgipty(apdtip, dstpdr, fvvfpp, fvvfgs, argcgi, fvvdfv,
031700070226     C+ fvvnfv, :In_PIG_num),
031800070129     C+ arglno, argerr,
031900070202     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp, arblna,
032000061221     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
032100061221     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
032200061221     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
032300061221     C+ from fnarb00f join
032400061220     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr,
032500070215     C+ apdtip, apdrsc, argcgi, argtgi, arglno, argerr,
032600070202     C+ ftdaas, ftdlnp, ftdnrs, ftdnsp,
032700061214     C+ ftdncl, ftdpkl, ftdvlu
032800061214     C+ from fiarg00f join
032900061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp, dstpdr, apdtip,
033000061214     C+ apdrsc, ftdaas, ftdlnp, ftdnrs, ftdnsp, ftdncl, ftdpkl, ftdvlu
033100061214     C+ from fiftd00f join
033200061214     C+ (select fvvfgs, fvvnpg, fvvnfv, fvvdfv, fvvfpp,
033300061214     C+ dstpdr, apdtip, apdrsc from fnfvv00f join
033400061214     C+ (select dstfgs, dstnfv, dstpdr, apdtip, apdrsc
033500061214     C+ from fndst00f join fiapd00f on dstpdr=apdpdr
033600070206     C+ where apdtip='A') as padroncini on
033700061214     C+ fvvfgs=dstfgs and fvvnfv=dstnfv where fvvnpg='4' and fvvfcf<>' '
033800061214     C+ and fvvdfv >= :DFV_DA  and fvvdfv <= :DFV_A
033900061214     C+ and fvvnfv >= :NFV_DA  and fvvnfv <= :NFV_A) as distinte on
034000070208     C+ fvvfgs=ftdfgs and fvvdfv=ftdddc and fvvnfv=ftdndc
034100061214     C+ where ftdtsr='C') as bolle on
034200061214     C+ ftdaas=argaas and ftdlnp=arglnp and
034300061220     C+ ftdnrs=argnrs and ftdnsp=argnsp) as estrazione on
034400061220     C+ ftdaas=arbaas and ftdlnp=arblnp and
034500061220     C+ ftdnrs=arbnrs and ftdnsp=arbnsp
034600061214     C/END-EXEC
034700061214     C*
034800061214     C                   ENDSR
034900070202     C*------------------------------------------------------------------------*
035000070205     C* REPDATINODFV - Routine di reperimento dati rispetto alle selezioni a video - no distinta
035100070202     C*------------------------------------------------------------------------*
035200070202     C     REPDATINODFV  BEGSR
035300070202     C*
035400070205     C/EXEC SQL
035500070205     C+ insert into qtemp/wfst940f
035600070608     C*+ select 0, ' ', 0, 0, ' ', 0, ' ', ' ', argcgi, argtgi, 0,
035700070608     C+ select :In_FGS, ' ', 0, 0, ' ', 0, ' ', ' ', argcgi, argtgi, 0,
035800070205     C+ arglno, argerr, arbaas, arblnp, arbnrs, arbnsp, arblna,
035900070205     C+ arbncl, rtvpes(arbncl, arbncp, arbpkc, arbpkf),
036000070205     C+ rtvvol(arbncl, arbncr, arbfvf, arbvlc, arbvlf),
036100070205     C+ arbrsd, arbind, arblod, arbcad, arbprd, arbznc
036200070205     C+ from filtra201/fnarb00f join filtra201/fiarg00f
036300070205     C+ on arbaas=argaas and arblnp=arglnp and arbnrs=argnrs and
036400070205     C+ arbnsp=argnsp
036500070205     C+ where arbndc=0
036600070205     C/END-EXEC
036700070202     C*
036800070202     C                   ENDSR
036900070202     C*------------------------------------------------------------------------*
037000070202     C* EXCLBOLL - Routine di esclusione bolle
037100070202     C*------------------------------------------------------------------------*
037200070202     C     EXCLBOLL      BEGSR
037300070205     C*
037400070202     C/EXEC SQL
037500070202     C+ declare C9 cursor for
037600070202     C+ select * from wfst940f
037700070202     C/END-EXEC
037800070202     C
037900070202     C/EXEC SQL
038000070202     C+ open C9
038100070202     C/END-EXEC
038200070202     C
038300070202     C/EXEC SQL
038400070202     C+ Fetch next from C9 into :DS_WST94_DET
038500070202     C/END-EXEC
038600070202     C
038700070202     C*
038800070202     C* Se reperito qualcosa => analizzo
038900070202     C                   dow       sqlcod = *zeros
039000070205     C*
039100070205     C                   movel     'N'           Escbolla          1
039200070202     C*
039300070202     C* Escludo bolle nn in "lira" del P.O. richiesto
039400070202     C     DET_LNA       lookup    Lin                                    99
039500070202     C                   if        not *IN99 = *on
039600070202     C                   eval      Escbolla='S'
039700070202     C                   endif
039800070202     C*
039900070202     C* Se lancio x bolle NN in distinta verifico consegnabilità d ogni bolla
040000070202     C                   if        In_DFV   = *zeros AND
040100070202     C                             In_NFV   = *zeros AND
040200070202     C                             Escbolla = 'N'
040300070202     C*
040400070202     C* Aggancio la bolla
040500070202     C     KEYarb01_c    chain     fnarb01l
040600070202     C                   if        %found(fnarb01l)
040700070202     C*
040800070202     C* Verifico consegnabilità bolla
040900070202     C                   exsr      escludi
041000070202     C*
041100070202     C                   endif
041200070202     C                   endif
041300070202     C*
041400070202     C* Se bolla da escludere => elimino il record corrente dalla tabella sql
041500070202     C                   if        Escbolla = 'S'
041600070202     C/EXEC SQL
041700070202     C+ delete from wfst940f where CURRENT OF C9
041800070202     C/END-EXEC
041900070202     C                   endif
042000070202     C*
042100070202     C* Proseguo con la lettura della tabella sql
042200070202     C/EXEC SQL
042300070202     C+ Fetch next from C9 into :DS_WST94_DET
042400070202     C/END-EXEC
042500070202     C*
042600070202     C                   enddo
042700070202     C*
042800070202     C/EXEC SQL
042900070202     C+ close C9
043000070202     C/END-EXEC
043100070202     C*
043200070202     C                   ENDSR
043300060516     C*------------------------------------------------------------------------*
043400061214     C* STA_NDC_CGI - Routine di stampa x distita/giro
043500060516     C*------------------------------------------------------------------------*
043600061214     C     STA_NDC_CGI   BEGSR
043700061215     C*
043800061215     C* Apertura printer-file
043900061215     C                   open      TIST94P
044000061220     C*
044100061220     C* Valorizzo titolo tabulato
044200070125     C                   eval      stttes1 = '   *** VERIFICA ABBINAMENTI ' +
044300070117     C                                       'DISTINTE/GIRI ***'
044400070201     C                   if        In_LIV = 'S'
044500070125     C                   eval      stttes2 = '(SINTETICA)'
044600070117     C                   else
044700070201     C                   eval      stttes2 = '(DETTAGLIO)'
044800070201     C                   endif
044900061222     C*
045000061222     C* Valorizzo i paramteri d lancio
045100070201     C                   eval      stppar = 'P.O.: ' +
045200070201     C                                       %trim(%editw(In_FGS:'0   '))
045300061222     C                   if        In_NFV <> *zeros
045400070116     C                   eval      stppar = %trim(stppar) + ' '+
045500070116     C                                      'PARAMETRI: tutti le bolle nella ' +
045600061222     C                                      'distinta '                        +
045700061222     C                                       %trim(%editw(In_FGS:'0   '))      +
045800061222     C                                       ' '                               +
045900061222     C                                       %trim(%editc(In_NFV:'4'))
046000061222     C                   else
046100070201     C                   if        In_DFV <> *zeros
046200070116     C                   eval      stppar = %trim(stppar) + ' '+
046300070116     C                                      'PARAMETRI: tutti le bolle nelle ' +
046400061222     C                                      'distinte in data '                +
046500061222     C                                      %editw(In_DFV_stp:'0  /  /    ')
046600070201     C                   else
046700070201     C                   eval      stppar = %trim(stppar) + ' '+
046800070201     C                                      'PARAMETRI: tutti le bolle non ' +
046900070201     C                                      'inserite in nessuna distinta'
047000070201     C                   endif
047100061222     C                   endif
047200061218     C*
047300061218     C* Se scelta stampa nn dettaglio stampo subito la testata
047400061220     C                   if        *in40 = *off
047500061218     C                   write     st94te
047600061222     C                   seton                                        60
047700061220     C                   write     st94dte
047800061220     C                   write     st94ri
047900061222     C                   setoff                                       05
048000061218     C                   endif
048100060516     C*
048200060516     C/EXEC SQL
048300061215     C+ declare C1 cursor for
048400061214     C+ select * from wfst940f
048500070205     C+ order by wst94nfv, wst94pty, wst94cgi,
048600070205     C+ wst94prd, wst94lod, wst94ind, wst94cad, wst94rsd
048700061214     C+ for read only
048800060516     C/END-EXEC
048900060516     C
049000060516     C/EXEC SQL
049100061215     C+ open C1
049200060516     C/END-EXEC
049300060516     C
049400060516     C/EXEC SQL
049500061215     C+ Fetch next from C1 into :DS_WST94_DET
049600060516     C/END-EXEC
049700060516     C*
049800060516     C* Se reperito qualcosa => analizzo
049900061214     C                   dow       sqlcod = *zeros
050000061220     C*
050100061220     C* Valorizzo i dati in stampa relativi alla distinta
050200061220     C*
050300061220     C* - codice distinta
050400061220     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
050500061220     C                                       ' ' +
050600061220     C                                       %trim(%editc(det_nfv:'4'))
050700061220     C* - autotrasportatore
050800061220     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
050900061220     C                                       ' ' +
051000061220     C                                       det_rsc
051100061220     C* - data distinta
051200061220     C                   z-add     det_dfv       G08INV
051300061220     C                   z-add     *ZEROS        G08DAT
051400061220     C                   movel     '3'           G08ERR
051500061220     C                   call      'XSRDA8'
051600061220     C                   parm                    WLBDA8
051700061220     C                   z-add     G08DAT        stpdat
051800061220     C* - prestazione distinta
051900061220     C                   if        det_fpp = 'M'
052000061220     C                   eval      stpprest = 'MAT.'
052100061220     C                   endif
052200061220     C                   if        det_fpp = 'P'
052300061220     C                   eval      stpprest = 'POM.'
052400061220     C                   endif
052500061218     C*
052600061218     C* Se scelta stampa anche dettaglio ad ogni distinta salto pagina
052700061220     C                   if        *in40 = *on
052800061220     C                   eval      stpdes = 'Distinta: '+
052900061220     C                                %trim(stpdist)+' del '+
053000061220     C                                %editw(stpdat:'0  /  /    ')+' '+stpprest+
053100061220     C                                '    '+'Autotrasportatore: '+
053200061220     C                                stpaut
053300061222     C   60              write     st94ri
053400061222     C   60              write     st94er
053500061220     C                   write     st94te
053600061222     C                   seton                                        60
053700061222     C                   setoff                                       05
053800061218     C                   endif
053900061218     C*
054000061220     C* Inizializzo l'indicatore d nuova distinta
054100061218     C                   setoff                                       51
054200061215     C*
054300061215     C* Salvo la distinta corrente
054400061215     C                   eval      SAV_NFV = DET_NFV
054500061215     C*
054600061215     C/EXEC SQL
054700061215     C+ declare C2 cursor for
054800061219     C+ select wst94nfv, count(*), decimal(sum(wst94ncl), 9, 0),
054900061221     C+ decimal(sum(wst94pes), 9, 0), decimal(sum(wst94vol), 9, 2)
055000061219     C+ from wfst940f
055100061219     C+ where wst94nfv = :SAV_NFV
055200061221     C+ group by wst94nfv
055300061215     C+ for read only
055400061215     C/END-EXEC
055500061215     C
055600061215     C/EXEC SQL
055700061215     C+ open C2
055800061215     C/END-EXEC
055900061215     C
056000061215     C/EXEC SQL
056100061218     C+ Fetch next from C2 into :DS_WST94_TOTD
056200061215     C/END-EXEC
056300061215     C
056400061215     C/EXEC SQL
056500061215     C+ close C2
056600061215     C/END-EXEC
056700061215     C*
056800061215     C* Ad ogni rottura d distinta reperisco il totale bolle in distinta
056900061215     C                   dow       sqlcod = *zeros AND
057000061215     C                             DET_NFV = SAV_NFV
057100061215     C*
057200061215     C* Salvo il giro corrente
057300061215     C                   eval      SAV_CGI = DET_CGI
057400061215     C*
057500061215     C/EXEC SQL
057600061215     C+ declare C3 cursor for
057700070129     C+ select wst94nfv, wst94cgi, count(*),
057800070129     C+ decimal(sum(wst94ncl), 9, 0),
057900061221     C+ decimal(sum(wst94pes), 9, 0),
058000061221     C+ decimal(sum(wst94vol)+0.005, 9, 2)
058100061219     C+ from wfst940f
058200061215     C+ where wst94nfv = :SAV_NFV and wst94cgi = :SAV_CGI
058300061221     C+ group by wst94nfv, wst94cgi
058400061215     C+ for read only
058500061215     C/END-EXEC
058600061215     C
058700061215     C/EXEC SQL
058800061215     C+ open C3
058900061215     C/END-EXEC
059000061215     C
059100061215     C/EXEC SQL
059200061215     C+ Fetch next from C3 into :DS_WST94_TOTG
059300061215     C/END-EXEC
059400061215     C
059500061215     C/EXEC SQL
059600061215     C+ close C3
059700061215     C/END-EXEC
059800061220     C*
059900061220     C* Valorizzo i dati in stampa relativi al giro
060000070202     C                   clear                   stpcgipty
060100061215     C                   eval      stpgiro   = det_cgi
060200070202     C* - decodifica giro e priorità
060300070125     C                   if        det_cgi <> *blanks
060400070215     C                   eval      stpcgipty = %editc(det_pty:'4')
060500070125     C                   clear                   fidg09ds
060600070125     C                   eval      d09iop0 = '001'
060700070202     C                   eval      d09ifgs = det_fgs
060800070125     C                   eval      d09idat = datcor
060900070125     C                   eval      d09icgi = det_cgi
061000070709     C                   eval      d09itug = 'C'
061100070125     C                   movel(p)  fidg09ds      KPJBU
061200070125     C                   call      'FIDG09R'
061300070125     C                   parm                    KPJBA
061400070125     C                   movel(p)  KPJBU         fidg09ds
061500070125     C                   if        d09oerr = *blanks
061600070205     C                   eval      stpain   = d09oain
061700070125     C                   eval      stpgirod = d09odes
061800070125     C                   else
061900070205     C                   eval      stpain   = *all'*'
062000070125     C                   eval      stpgirod = *all'*'
062100070125     C                   endif
062200070125     C*
062300070125     C                   else
062400070215     C                   eval      stpain    = *blanks
062500070215     C                   eval      stpgirod  = '*** non attribuito ***'
062600070215     C                   eval      stpcgipty = '--'
062700070125     C                   endif
062800070202     C*
062900070202     C                   exsr      valdet
063000061218     C*
063100061218     C* Gestisco salto pagina
063200061218     C                   if        *in05 = *on
063300061222     C   60              write     st94ri
063400061222     C   60              write     st94er
063500061218     C                   write     st94te
063600061222     C                   seton                                        60
063700061220     C  N40              write     st94dte
063800061220     C  N40              write     st94ri
063900061218     C                   setoff                                       05
064000061218     C*
064100061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
064200061220     C                   if        *in40 = *on
064300061220     C                   write     st94ri
064400061220     C                   write     st94dtd
064500061220     C                   write     st94ri
064600070202     C                   write     st94gtd
064700070202     C                   write     st94ri
064800061218     C                   endif
064900061218     C                   endif
065000061219     C*
065100061219     C* Se richiesta stampa in dettaglio lascio 1 riga vuota ad ogni cambio giro
065200061220     C                   if        *in40 = *on
065300061219     C   51              write     st94sp
065400061219     C                   endif
065500061218     C*
065600061218     C* Quindi stampo riga totali x distinta/giro
065700070202     C                   if        *in40 = *off
065800070202     C                   if        *in51 = *off
065900070202     C                   write     st94dt
066000070202     C                   seton                                        51
066100070202     C                   exsr      valdet
066200070202     C                   write     st94dtg
066300070202     C                   else
066400070202     C                   write     st94dtg
066500070202     C                   endif
066600070202     C                   endif
066700070202     C*
066800070202     C   40              write     st94ri
066900070202     C   40              write     st94dtd
067000070202     C   40              write     st94ri
067100070202     C   40              write     st94gtd
067200070202     C   40              write     st94ri
067300061218     C                   seton                                        51
067400061215     C*
067500061215     C* Ad ogni rottura d giro reperisco i totali bolle, colli. peso, volume x distinta/giro
067600061219     C                   dow       sqlcod = *zeros   AND
067700061219     C                             DET_NFV = SAV_NFV AND
067800061215     C                             DET_CGI = SAV_CGI
067900061215     C*
068000061215     C* Se scelta stampa anche dettaglio => provvedo
068100061220     C                   if        *in40 = *on
068200061222     C                   eval      stdsped = %editc(det_aas:'4') +
068300061220     C                                       ' ' +
068400070116     C                                       %trim(%editw(det_lnp:'0   ')) +
068500061220     C                                       ' ' +
068600061222     C                                       %editc(det_nrs:'4') +
068700061220     C                                       ' ' +
068800061222     C                                       %editc(det_nsp:'4')
068900070215     C                   if        det_tgi = 'M'
069000070215     C                   eval      stdtgi = 'M'
069100070215     C                   else
069200070215     C                   eval      stdtgi = *blanks
069300070215     C                   endif
069400061220     C                   eval      stdrsd = det_rsd
069500061220     C                   eval      stdind = det_ind
069600061220     C                   eval      stdloc = det_lod
069700061220     C                   eval      stdcap = det_cad
069800061221     C                   eval      stdprd = det_prd
069900061220     C***                eval      stdznc = det_znc
070000070125     C                   eval      stdncl =
070100070605     C                                       %subst(%editc(det_ncl:'2'):1:6)
070200070125     C                   eval      stdpkf =
070300061221     C                                       %subst(%editc(det_pes:'2'):3:7)
070400070125     C                   eval      stdvlf = %editc(det_vol:'2')
070500061222     C*
070600061222     C* Gestisco eventuale errore "TELLUS"
070700061222     C                   setoff                                       53
070800070102     C                   if        det_cgi =  *blanks AND
070900070102     C                             det_err <> '10'
071000061222     C                   seton                                        53
071100070102     C                   if        det_err = *blanks
071200070102     C                   eval      stderr = '(' + %subst(det_lno:1:2) + ')'
071300061222     C                   else
071400070102     C                   eval      stderr = '(' + det_err + ')'
071500061222     C                   endif
071600061222     C                   endif
071700061218     C*
071800061218     C* Gestisco salto pagina
071900061218     C                   if        *in05 = *on
072000061222     C   60              write     st94ri
072100061222     C   60              write     st94er
072200061218     C                   write     st94te
072300061222     C                   seton                                        60
072400061220     C  N40              write     st94dte
072500061220     C  N40              write     st94ri
072600061218     C                   setoff                                       05
072700061218     C*
072800061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
072900061220     C                   if        *in40 = *on
073000061220     C                   write     st94ri
073100061220     C                   write     st94dtd
073200061220     C                   write     st94ri
073300070202     C                   write     st94gtd
073400070202     C                   write     st94ri
073500061218     C                   endif
073600061218     C                   endif
073700061215     C*
073800061215     C* Stampo riga d dettaglio bolle
073900061215     C                   write     st94dd
074000061215     C                   endif
074100060516     C*
074200061214     C* Proseguo con la lettura del recordset
074300061214     C/EXEC SQL
074400061215     C+ Fetch next from C1 into :DS_WST94_DET
074500061214     C/END-EXEC
074600061214     C                   enddo
074700061215     C*
074800061215     C                   enddo
074900070117     C*
075000070117     C* Totalizzo le bolle
075100070117     C                   eval      wTotBolle = wTotBolle + totd_bol
075200061215     C*
075300061215     C                   enddo
075400060516     C*
075500060516     C/EXEC SQL
075600061215     C+ close C1
075700060516     C/END-EXEC
075800070117     C*
075900070117     C                   evalr     stototbol = %editc(wTotBolle:'2')
076000061215     C*
076100061215     C* Chiusura printer-file
076200061220     C                   write     st94ri
076300070117     C                   write     st94tt
076400070117     C                   write     st94ri
076500061222     C                   write     st94er
076600061220     C                   write     st94bk
076700061220     C                   write     st94bk
076800061215     C                   write     st94fi
076900061215     C                   close     TIST94P
077000060516     C*
077100060516     C                   ENDSR
077200070215     C*------------------------------------------------------------------------*
077300070215     C* STA_NDC_PTY - Routine di stampa x distita/priorità (solo totali)
077400070215     C*------------------------------------------------------------------------*
077500070215     C     STA_NDC_PTY   BEGSR
077600070215     C*
077700070215     C* Apertura printer-file
077800070215     C                   open      TIST94P
077900070215     C*
078000070215     C* Valorizzo titolo tabulato
078100070215     C                   eval      stttes1 = '*** VERIFICA ABBINAMENTI ' +
078200070215     C                                       'DISTINTE/PRIORITA ***'
078300070215     C                   eval      stttes2 = ' (TOTALI) '
078400070215     C*
078500070215     C* Valorizzo i paramteri d lancio
078600070215     C                   eval      stppar = 'P.O.: ' +
078700070215     C                                       %trim(%editw(In_FGS:'0   '))
078800070215     C                   if        In_NFV <> *zeros
078900070215     C                   eval      stppar = %trim(stppar) + ' '+
079000070215     C                                      'PARAMETRI: tutti le bolle nella ' +
079100070215     C                                      'distinta '                        +
079200070215     C                                       %trim(%editw(In_FGS:'0   '))      +
079300070215     C                                       ' '                               +
079400070215     C                                       %trim(%editc(In_NFV:'4'))
079500070215     C                   else
079600070215     C                   if        In_DFV <> *zeros
079700070215     C                   eval      stppar = %trim(stppar) + ' '+
079800070215     C                                      'PARAMETRI: tutti le bolle nelle ' +
079900070215     C                                      'distinte in data '                +
080000070215     C                                      %editw(In_DFV_stp:'0  /  /    ')
080100070215     C                   else
080200070215     C                   eval      stppar = %trim(stppar) + ' '+
080300070215     C                                      'PARAMETRI: tutti le bolle non ' +
080400070215     C                                      'inserite in nessuna distinta'
080500070215     C                   endif
080600070215     C                   endif
080700070215     C*
080800070215     C* Se scelta stampa nn dettaglio stampo subito la testata
080900070215     C                   if        *in40 = *off
081000070215     C                   write     st94te
081100070215     C                   seton                                        60
081200070215     C                   write     st94dte
081300070215     C                   write     st94ri
081400070215     C                   setoff                                       05
081500070215     C                   endif
081600070215     C*
081700070215     C/EXEC SQL
081800070215     C+ declare C10 cursor for
081900070215     C+ select * from wfst940f
082000070215     C+ order by wst94nfv, wst94pty
082100070215     C+ for read only
082200070215     C/END-EXEC
082300070215     C
082400070215     C/EXEC SQL
082500070215     C+ open C10
082600070215     C/END-EXEC
082700070215     C
082800070215     C/EXEC SQL
082900070215     C+ Fetch next from C10 into :DS_WST94_DET
083000070215     C/END-EXEC
083100070215     C*
083200070215     C* Se reperito qualcosa => analizzo
083300070215     C                   dow       sqlcod = *zeros
083400070215     C*
083500070215     C* Valorizzo i dati in stampa relativi alla distinta
083600070215     C*
083700070215     C* - codice distinta
083800070215     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
083900070215     C                                       ' ' +
084000070215     C                                       %trim(%editc(det_nfv:'4'))
084100070215     C* - autotrasportatore
084200070215     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
084300070215     C                                       ' ' +
084400070215     C                                       det_rsc
084500070215     C* - data distinta
084600070215     C                   z-add     det_dfv       G08INV
084700070215     C                   z-add     *ZEROS        G08DAT
084800070215     C                   movel     '3'           G08ERR
084900070215     C                   call      'XSRDA8'
085000070215     C                   parm                    WLBDA8
085100070215     C                   z-add     G08DAT        stpdat
085200070215     C* - prestazione distinta
085300070215     C                   if        det_fpp = 'M'
085400070215     C                   eval      stpprest = 'MAT.'
085500070215     C                   endif
085600070215     C                   if        det_fpp = 'P'
085700070215     C                   eval      stpprest = 'POM.'
085800070215     C                   endif
085900070215     C*
086000070215     C* Inizializzo l'indicatore d nuova distinta
086100070215     C                   setoff                                       51
086200070215     C*
086300070215     C* Salvo la distinta corrente
086400070215     C                   eval      SAV_NFV = DET_NFV
086500070215     C*
086600070215     C/EXEC SQL
086700070215     C+ declare C11 cursor for
086800070215     C+ select wst94nfv, count(*),
086900070215     C+ decimal(sum(wst94ncl), 9, 0),
087000070215     C+ decimal(sum(wst94pes), 9, 0),
087100070215     C+ decimal(sum(wst94vol), 9, 2)
087200070215     C+ from wfst940f
087300070215     C+ where wst94nfv = :SAV_NFV
087400070215     C+ group by wst94nfv
087500070215     C+ for read only
087600070215     C/END-EXEC
087700070215     C
087800070215     C/EXEC SQL
087900070215     C+ open C11
088000070215     C/END-EXEC
088100070215     C
088200070215     C/EXEC SQL
088300070215     C+ Fetch next from C11 into :DS_WST94_TOTD
088400070215     C/END-EXEC
088500070215     C
088600070215     C/EXEC SQL
088700070215     C+ close C11
088800070215     C/END-EXEC
088900070215     C*
089000070215     C* Ad ogni rottura d distinta reperisco il totale bolle in distinta
089100070215     C                   dow       sqlcod = *zeros AND
089200070215     C                             DET_NFV = SAV_NFV
089300070215     C*
089400070215     C* Salvo il giro corrente
089500070215     C                   eval      SAV_PTY = DET_PTY
089600070215     C*
089700070215     C/EXEC SQL
089800070215     C+ declare C12 cursor for
089900070215     C+ select wst94nfv, wst94pty, count(*),
090000070215     C+ decimal(sum(wst94ncl), 9, 0),
090100070215     C+ decimal(sum(wst94pes), 9, 0),
090200070215     C+ decimal(sum(wst94vol)+0.005, 9, 2)
090300070215     C+ from wfst940f
090400070215     C+ where wst94nfv = :SAV_NFV and wst94pty = :SAV_PTY
090500070215     C+ group by wst94nfv, wst94pty
090600070215     C+ for read only
090700070215     C/END-EXEC
090800070215     C
090900070215     C/EXEC SQL
091000070215     C+ open C12
091100070215     C/END-EXEC
091200070215     C
091300070215     C/EXEC SQL
091400070215     C+ Fetch next from C12 into :DS_WST94_TOTG
091500070215     C/END-EXEC
091600070215     C
091700070215     C/EXEC SQL
091800070215     C+ close C12
091900070215     C/END-EXEC
092000070215     C*
092100070215     C* Valorizzo i dati in stampa relativi al giro
092200070215     C                   clear                   stpcgipty
092300070215     C                   eval      stpgiro   = *blanks
092400070215     C* - decodifica giro e priorità
092500070215     C                   if        det_cgi <> *blanks
092600070215     C                   eval      stpcgipty = %editc(det_pty:'4')
092700070215     C                   else
092800070215     C                   eval      stpcgipty = '--'
092900070215     C                   endif
093000070215     C*
093100070215     C                   exsr      valdet
093200070215     C*
093300070215     C* Gestisco salto pagina
093400070215     C                   if        *in05 = *on
093500070215     C   60              write     st94ri
093600070215     C   60              write     st94er
093700070215     C                   write     st94te
093800070215     C                   seton                                        60
093900070215     C  N40              write     st94dte
094000070215     C  N40              write     st94ri
094100070215     C                   setoff                                       05
094200070215     C                   endif
094300070215     C*
094400070215     C* Quindi stampo riga totali x distinta/priorità
094500070215     C                   if        *in51 = *off
094600070215     C                   write     st94dt
094700070215     C                   seton                                        51
094800070215     C                   exsr      valdet
094900070215     C                   write     st94dtg
095000070215     C                   else
095100070215     C                   write     st94dtg
095200070215     C                   endif
095300070215     C                   seton                                        51
095400070215     C*
095500070215     C* Ad ogni rottura d giro reperisco i totali bolle, colli. peso, volume x distinta/giro
095600070215     C                   dow       sqlcod = *zeros   AND
095700070215     C                             DET_NFV = SAV_NFV AND
095800070215     C                             DET_PTY = SAV_PTY
095900070215     C*
096000070215     C* Proseguo con la lettura del recordset
096100070215     C/EXEC SQL
096200070215     C+ Fetch next from C10 into :DS_WST94_DET
096300070215     C/END-EXEC
096400070215     C                   enddo
096500070215     C*
096600070215     C                   enddo
096700070215     C*
096800070215     C* Totalizzo le bolle
096900070215     C                   eval      wTotBolle = wTotBolle + totd_bol
097000070215     C*
097100070215     C                   enddo
097200070215     C*
097300070215     C/EXEC SQL
097400070215     C+ close C10
097500070215     C/END-EXEC
097600070215     C*
097700070215     C                   evalr     stototbol = %editc(wTotBolle:'2')
097800070215     C*
097900070215     C* Chiusura printer-file
098000070215     C                   write     st94ri
098100070215     C                   write     st94tt
098200070215     C                   write     st94ri
098300070215     C                   write     st94er
098400070215     C                   write     st94bk
098500070215     C                   write     st94bk
098600070215     C                   write     st94fi
098700070215     C                   close     TIST94P
098800070215     C*
098900070215     C                   ENDSR
099000070202     C*------------------------------------------------------------------------*
099100070202     C* VALDET - Routine valorizzazione dettaglio in stampa
099200070202     C*------------------------------------------------------------------------*
099300070202     C     VALDET        BEGSR
099400070202     C*
099500070202     C* Se sintetica....
099600070202     C                   if        *in40 = *off
099700070202     C   51              evalr     stptotbol =
099800070605     C                             %subst(%editc(totg_bol:'1'):7:5)
099900070202     C   51              eval      stpperc   = totg_bol * 100 / totd_bol
100000070202     C   51              evalr     stptotncl =
100100070202     C                             %subst(%editc(totg_ncl:'1'):7:5)
100200070202     C   51              evalr     stptotpkf =
100300070202     C                             %subst(%editc(totg_pes:'1'):6:6)
100400070202     C   51              evalr     stptotvlf =
100500070202     C                             %subst(%editc(totg_vol:'1'):8:5)
100600070202     C*
100700070202     C  N51              evalr     stptotbol =
100800070605     C                             %subst(%editc(totd_bol:'1'):7:5)
100900070202     C  N51              eval      stpperc   = *zeros
101000070202     C  N51              evalr     stptotncl =
101100070202     C                             %subst(%editc(totd_ncl:'1'):7:5)
101200070202     C  N51              evalr     stptotpkf =
101300070202     C                             %subst(%editc(totd_pes:'1'):6:6)
101400070202     C  N51              evalr     stptotvlf =
101500070202     C                             %subst(%editc(totd_vol:'1'):8:5)
101600070202     C                   else
101700070202     C                   evalr     stptotbol =
101800070605     C                             %subst(%editc(totg_bol:'1'):7:5) + '/' +
101900070605     C                             %subst(%editc(totd_bol:'1'):7:5)
102000070202     C                   eval      stpperc   = totg_bol * 100 / totd_bol
102100070202     C                   evalr     stptotncl =
102200070202     C                             %subst(%editc(totg_ncl:'1'):7:5) + '/' +
102300070202     C                             %subst(%editc(totd_ncl:'1'):7:5)
102400070202     C                   evalr     stptotpkf =
102500070202     C                             %subst(%editc(totg_pes:'1'):6:6) + '/' +
102600070202     C                             %subst(%editc(totd_pes:'1'):6:6)
102700070202     C                   evalr     stptotvlf =
102800070202     C                             %subst(%editc(totg_vol:'1'):8:5) + '/' +
102900070202     C                             %subst(%editc(totd_vol:'1'):8:5)
103000070202     C                   endif
103100070202     C*
103200070202     C                   ENDSR
103300061220     C*------------------------------------------------------------------------*
103400061220     C* STA_CGI - Routine di stampa x giro
103500061220     C*------------------------------------------------------------------------*
103600061220     C     STA_CGI       BEGSR
103700061220     C*
103800061220     C* Apertura printer-file
103900061220     C                   open      TIST94P
104000061220     C*
104100061220     C* Valorizzo titolo tabulato
104200070125     C                   eval      stttes1 = '      *** VERIFICA ASSORBIMENTO '+
104300070117     C                                       'GIRI ***'
104400070201     C                   if        In_LIV = 'S'
104500070201     C                   eval      stttes2 = '(SINTETICA)'
104600070201     C                   else
104700070201     C                   eval      stttes2 = '(DETTAGLIO)'
104800070201     C                   endif
104900061222     C*
105000061222     C* Valorizzo i paramteri d lancio
105100070116     C                   eval      stppar = 'P.O.: ' +
105200070116     C                                       %trim(%editw(In_FGS:'0   '))
105300070201     C                   if        In_DFV <> *zeros
105400070201     C                   eval      stppar = %trim(stppar) + ' '+
105500070201     C                                      'PARAMETRI: tutti le bolle nelle ' +
105600070201     C                                      'distinte in data '                +
105700070201     C                                      %editw(In_DFV_stp:'0  /  /    ')
105800070201     C                   else
105900070201     C                   eval      stppar = %trim(stppar) + ' '+
106000070201     C                                      'PARAMETRI: tutti le bolle non ' +
106100070201     C                                      'inserite in nessuna distinta'
106200070201     C                   endif
106300061220     C*
106400061220     C* Se scelta stampa nn dettaglio stampo subito la testata
106500061220     C                   if        *in40 = *off
106600061220     C                   write     st94te
106700061222     C                   seton                                        60
106800061220     C                   write     st94gte
106900061220     C                   write     st94ri
107000061222     C                   setoff                                       05
107100061220     C                   endif
107200061220     C*
107300061220     C/EXEC SQL
107400061220     C+ declare C7 cursor for
107500061220     C+ select * from wfst940f
107600061221     C+ order by wst94cgi, wst94prd, wst94lod,
107700061221     C+ wst94ind, wst94cad, wst94rsd
107800061220     C+ for read only
107900061220     C/END-EXEC
108000061220     C
108100061220     C/EXEC SQL
108200061220     C+ open C7
108300061220     C/END-EXEC
108400061220     C
108500070202     C/EXEC SQL
108600070202     C+ Fetch next from C7 into :DS_WST94_DET
108700070202     C/END-EXEC
108800070202     C
108900061220     C*
109000061220     C* Se reperito qualcosa => analizzo
109100070202     C                   dow       sqlcod = *zeros
109200061220     C*
109300061220     C* Inizializzo l'indicatore d nuovo giro
109400061220     C                   setoff                                       51
109500061220     C*
109600061220     C* Se scelta stampa anche dettaglio ad ogni giro salto pagina
109700061220     C                   if        *in40 = *on
109800061222     C   60              write     st94ri
109900061222     C   60              write     st94er
110000061220     C                   write     st94te
110100061222     C                   seton                                        60
110200061222     C                   setoff                                       05
110300061220     C                   endif
110400061220     C*
110500061220     C* Salvo il giro corrente
110600061220     C                   eval      SAV_CGI = DET_CGI
110700061220     C*
110800061220     C/EXEC SQL
110900061220     C+ declare C8 cursor for
111000061220     C+ select min(wst94nfv), wst94cgi, count(*),
111100061221     C+ decimal(sum(wst94ncl), 9, 0), decimal(sum(wst94pes), 9, 0),
111200061221     C+ decimal(sum(wst94vol), 9, 2) from wfst940f
111300061220     C+ where wst94cgi = :SAV_CGI
111400061221     C+ group by wst94cgi
111500061220     C+ for read only
111600061220     C/END-EXEC
111700061220     C
111800061220     C/EXEC SQL
111900061220     C+ open C8
112000061220     C/END-EXEC
112100061220     C
112200070202     C
112300070202     C*
112400061220     C/EXEC SQL
112500061220     C+ Fetch next from C8 into :DS_WST94_TOTG
112600061220     C/END-EXEC
112700061220     C
112800061220     C/EXEC SQL
112900061220     C+ close C8
113000061220     C/END-EXEC
113100061220     C*
113200061220     C* Valorizzo i dati in stampa relativi al giro
113300061220     C*
113400061220     C* - codice giro
113500061220     C                   eval      stpgiro   = det_cgi
113600070125     C* - decodifica giro
113700070125     C                   if        det_cgi <> *blanks
113800070125     C                   clear                   fidg09ds
113900070125     C                   eval      d09iop0 = '001'
114000070202     C                   eval      d09ifgs = det_fgs
114100070125     C                   eval      d09idat = datcor
114200070125     C                   eval      d09icgi = det_cgi
114300070709     C                   eval      d09itug = 'C'
114400070125     C                   movel(p)  fidg09ds      KPJBU
114500070125     C                   call      'FIDG09R'
114600070125     C                   parm                    KPJBA
114700070125     C                   movel(p)  KPJBU         fidg09ds
114800070125     C                   if        d09oerr = *blanks
114900070205     C                   eval      stpain   = d09oain
115000070205     C                   eval      stpgirod = d09odes
115100070125     C                   else
115200070205     C                   eval      stpain   = *all'*'
115300070205     C                   eval      stpgirod = *all'*'
115400070125     C                   endif
115500070125     C*
115600070125     C                   else
115700070205     C                   eval      stpain   = *blanks
115800070125     C                   eval      stpgirod = '*** non attribuito ***'
115900070125     C                   endif
116000061220     C* - totali
116100070125     C                   evalr     stptotbol =
116200070605     C                             %subst(%editc(totg_bol:'1'):7:5)
116300070125     C                   evalr     stptotncl =
116400061220     C                             %subst(%editc(totg_ncl:'1'):7:5)
116500070125     C                   evalr     stptotpkf =
116600061221     C                             %subst(%editc(totg_pes:'1'):6:6)
116700070125     C                   evalr     stptotvlf =
116800061221     C                             %subst(%editc(totg_vol:'1'):8:5)
116900061220     C*
117000061220     C* Gestisco salto pagina
117100061220     C                   if        *in05 = *on
117200061222     C   60              write     st94ri
117300061222     C   60              write     st94er
117400061220     C                   write     st94te
117500061222     C                   seton                                        60
117600061220     C  N40              write     st94gte
117700070125     C   40              write     st94gtt
117800070125     C   40              write     st94ri
117900070125     C   40              write     st94gtd
118000061220     C                   write     st94ri
118100061220     C                   setoff                                       05
118200061220     C                   endif
118300061220     C*
118400061220     C* Stampo il giro corrente
118500070125     C   40              write     st94gtt
118600070125     C   40              write     st94ri
118700070125     C   40              write     st94gtd
118800061220     C   40              write     st94ri
118900061220     C  N40              write     st94gt
119000061220     C  N40              setoff                                       51
119100061220     C*
119200061220     C* Ad ogni rottura d giro se richiesto stampa il dettaglio bolle
119300061220     C                   dow       sqlcod = *zeros AND
119400061220     C                             DET_CGI = SAV_CGI
119500061220     C*
119600061220     C* Gestisco salto pagina
119700061220     C                   if        *in05 = *on
119800061222     C   60              write     st94ri
119900061222     C   60              write     st94er
120000061220     C                   write     st94te
120100061222     C                   seton                                        60
120200061220     C  N40              write     st94gte
120300070125     C   40              write     st94gtt
120400070125     C   40              write     st94ri
120500070125     C   40              write     st94gtd
120600061220     C                   write     st94ri
120700061220     C                   setoff                                       05
120800061220     C                   endif
120900061220     C*
121000061220     C* Se scelta stampa anche dettaglio => provvedo
121100061220     C                   if        *in40 = *on
121200061222     C                   eval      stdsped = %editc(det_aas:'4') +
121300061220     C                                       ' ' +
121400070116     C                                       %trim(%editw(det_lnp:'0   ')) +
121500061220     C                                       ' ' +
121600061222     C                                       %editc(det_nrs:'4') +
121700061220     C                                       ' ' +
121800061222     C                                       %editc(det_nsp:'4')
121900070215     C                   if        det_tgi = 'M'
122000070215     C                   eval      stdtgi = 'M'
122100070215     C                   else
122200070215     C                   eval      stdtgi = *blanks
122300070215     C                   endif
122400061220     C                   eval      stdrsd = det_rsd
122500061220     C                   eval      stdind = det_ind
122600061220     C                   eval      stdloc = det_lod
122700061220     C                   eval      stdcap = det_cad
122800061221     C                   eval      stdprd = det_prd
122900061220     C***                eval      stdznc = det_znc
123000070125     C                   eval      stdncl =
123100070605     C                                       %subst(%editc(det_ncl:'2'):1:6)
123200070125     C                   eval      stdpkf =
123300061221     C                                       %subst(%editc(det_pes:'2'):3:7)
123400070125     C                   eval      stdvlf = %editc(det_vol:'2')
123500070102     C*
123600070102     C* Gestisco eventuale errore "TELLUS"
123700070102     C                   setoff                                       53
123800070102     C                   if        det_cgi =  *blanks AND
123900070102     C                             det_err <> '10'
124000070102     C                   seton                                        53
124100070102     C                   if        det_err = *blanks
124200070102     C                   eval      stderr = '(' + %subst(det_lno:1:2) + ')'
124300070102     C                   else
124400070102     C                   eval      stderr = '(' + det_err + ')'
124500070102     C                   endif
124600070102     C                   endif
124700061220     C*
124800061220     C* Stampo riga d dettaglio bolle
124900061220     C                   write     st94dd
125000061220     C                   endif
125100070202     C/EXEC SQL
125200070202     C+ Fetch next from C7 into :DS_WST94_DET
125300070202     C/END-EXEC
125400070202     C*
125500061220     C                   enddo
125600070117     C*
125700070117     C* Totalizzo le bolle
125800070117     C                   eval      wTotBolle = wTotBolle + totg_bol
125900061220     C*
126000061220     C                   enddo
126100061220     C*
126200061220     C/EXEC SQL
126300061220     C+ close C7
126400061220     C/END-EXEC
126500070117     C*
126600070117     C                   evalr     stototbol = %editc(wTotBolle:'2')
126700070117     C*
126800070117     C* Chiusura printer-file
126900070117     C                   write     st94ri
127000070117     C                   write     st94tt
127100061220     C                   write     st94ri
127200061222     C                   write     st94er
127300061220     C                   write     st94bk
127400061220     C                   write     st94bk
127500061220     C                   write     st94fi
127600061220     C                   close     TIST94P
127700061220     C*
127800061220     C                   ENDSR
127900061215     C*--------------------------------------------------------------------------------------------*
128000061215     C* REPERISCE I DATI UTENTE
128100061215     C*--------------------------------------------------------------------------------------------*
128200061215     C     REPDATIUTE    BEGSR
128300061215     C*
128400061215     C* INIZIALIZZA VARIABILI DI WRK
128500061215     C                   CLEAR                   TIBS34DS
128600061215     C                   CLEAR                   AZUTEDS
128700061215     C                   CLEAR                   DDATIUTE
128800061215     C*
128900061215     C     *DTAARA       DEFINE    §azute        azuteds
129000061215     C     *DTAARA       DEFINE    §datiute      ddatiute
129100061215     C                   IN(E)     *DTAARA
129200061215     C                   IF        %Error
129300061215     c                   EVAL      I34Tla = 'L'
129400061215     C                   CALL      'TIBS34R'
129500061215     C                   PARM                    Tibs34Ds
129600061215     C                   IN        *DTAARA
129700061215     C                   ENDIF
129800061215     C*
129900061215     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
130000061215     C                   MOVEL(P)  RSUT          DSFIRS           20
130100061215     C*
130200061215     C                   ENDSR
130300070202     c*------------------------------------------------------------------
130400070202     c* Esclusione delle spedizioni da non passare a Tellus (presa da *pgm Elisa FNLRL1R)
130500070202     c*------------------------------------------------------------------
130600070202     c     ESCLUDI       BEGSR
130700070202     c*
130800070202     c                   clear                   Escbolla
130900070202     c*
131000070202     c* 1) Escludo bolle già consegnate
131100070202     c                   if        arbdcm > 0
131200070202     c                   eval      Escbolla='S'
131300070202     c                   goto      Endescludi
131400070202     c                   endif
131500070202     c*
131600070205     c* 2) Spedizioni già in distinta
131700070202     c* 3) Spedizioni in Fermo deposito
131800070202     c* 4) Spedizioni bloccate ("*" è per il cambio di porto)
131900070205     c                   if        (arbndc>0 and arbddc>0) or
132000070205     c                             arbffd<>' '             or
132100070205     c                             arbfbc='B' or arbfbc='*'
132200070202     c                   eval      Escbolla='S'
132300070202     c                   goto      Endescludi
132400070202     c                   endif
132500070202     c*
132600070202     c* 5) Tipo bolla di recupero
132700070202     c     arbcbo        lookup    REC                                    35
132800070202     c                   if        *in35
132900070202     c                   eval      Escbolla='S'
133000070202     c                   goto      Endescludi
133100070202     c                   endif
133200070202     c
133300070202     c* 6) con C.A. aperta
133400070202     c                   clear                   fidn12ds
133500070202     c                   eval      i12aas = arbaas
133600070202     c                   eval      i12lnp = arblnp
133700070202     c                   eval      i12nrs = arbnrs
133800070202     c                   eval      i12nsp = arbnsp
133900070202     c                   eval      i12fel = 'E'
134000070202     c                   eval      i12fan = 'E'
134100070202      *
134200070202     c                   call      'FIDN12R'
134300070202     c                   parm                    fidn12ds
134400070202      *
134500070202      * se non ci sono errori
134600070202     c                   if        o12err = ' '
134700070202      * se numero di CA trovate maggiore di zero
134800070202     c                   if        o12nca > 0
134900070202     c
135000070202     c                   z-add     *zeros        ii                2 0
135100070202     c     1             do        o12nca        ii
135200070202     c                   movea     skkey(ii)     dskey
135300070202     C                   Z-ADD     skdch(ii)     dctdch            8 0
135400070202     C                   movel     skcch(ii)     dctcch            2
135500070202      *
135600070202     C* -SE ESISTE C.A. APERTA  --> ESCO
135700070202     c                   If        dctdch = 0
135800070202     c                   eval      Escbolla='S'
135900070202     c                   leave
136000070202     c                   endif
136100070202     c
136200070202     c* Se c.a. chiusa con causale che non prevede consegna esco
136300070202     c                   if        dctcch<>*blanks
136400070202     c     dctcch        lookup    cchco                                  35
136500070202     c                   if        not *in35
136600070202     c                   eval      Escbolla='S'
136700070202     c                   leave
136800070202     c                   endif
136900070202     c                   endif
137000070202     c*
137100070202     C                   enddo
137200070202     c*
137300070202     c                   endif
137400070202     c                   endif
137500070202     C*
137600070202     C* -SE ESISTE C.A. APERTA  --> ESCO
137700070202    1C     Escbolla      IFEQ      'S'
137800070202     c                   goto      Endescludi
137900070202    1C                   ENDIF
138000070202     c*
138100070202     c* 7) Senza testare il record di giacenza, escludo tutte le bolle
138200070202     c*    con blocco G
138300070202    1c                   if        arbfbc='G'
138400070202     c                   eval      Escbolla='S'
138500070202     c                   goto      Endescludi
138600070202    3C                   endif
138700070202     C*
138800070202     c     Endescludi    ENDSR
138900070202     C*--------------------------------------------------------------------------------------------*
139000070202     C* CARTAB - Caricamento dati tabellati (logiche prese da *pgm Elisa FNLRL1R)
139100070202     C*--------------------------------------------------------------------------------------------*
139200070202     C     CARTAB        BEGSR
139300070202     C*
139400070202     c* Carico le causali chiusura c,a, che prevedono la consegna della merce
139500070202     c                   z-add     *zeros        xx                3 0
139600070202     C                   MOVEL     'CCH'         wTBEcod
139700070202     c     wTBEcod       chain     tntbe000                           31
139800070202      *
139900070202     c                   DOW       *in31 = *off
140000070202     c                   movel     TBEuni        DCCH
140100070202      * Se il S.I. è indicato deve essere uguale al mio, l'anomalia non deve essere per gli Eventi
140200070202     c                   IF        (TBEsif = knsif  or  TBEsif = *blanks)
140300070202     c                             and TBEatb = *BLANKS  and  §cchccco= 'S'
140400070202     c                   add       1             xx
140500070202     c                   movel     TBEke1        cchco(XX)
140600070202     c                   endif
140700070202     c     wTBEcod       reade     tntbe000                               31
140800070202     c                   enddo
140900070202     c
141000070202     c* Carico tipi record di recupero
141100070202     C                   Z-ADD     1             KTBKUT
141200070202     C                   MOVE      '3A'          KTBCOD
141300070202     C                   CLEAR                   IY                4 0
141400070202     C     KEYtab_c      SETLL     TABEL00F
141500070202     C     KEYtab_c      READE     TABEL00F                               98
141600070202     C     *IN98         DOWEQ     *OFF
141700070202     C                   MOVEL     TBLUNI        DS3a
141800070202     C     §3arbl        IFEQ      'R'
141900070202     C                   ADD       1             IY
142000070202     C                   MOVEL     TBLKEY        rec(IY)
142100070202     C                   ENDIF
142200070202     C     KEYtab_c      READE     TABEL00F                               98
142300070202     C                   ENDDO
142400070202     C*
142500070202     C                   ENDSR
142600070202     C*--------------------------------------------------------------------------------------------*
142700070202     C* CARICA I P.O. IN "lira" richiesta
142800070202     C*--------------------------------------------------------------------------------------------*
142900070202     C     CARLIRA       BEGSR
143000070202     C*
143100070202     C* CARICO TABELLA FILIALI GESTITE £6
143200070202     C                   clear                   trul06ds
143300070202     C                   move      '£6'          d06cod
143400070205     C                   movel     In_FGS        d06key
143500070202     C                   movel     trul06ds      kpjbu
143600070202     C                   call      'TRUL06R'
143700070202     C                   parm                    kpjba
143800070202     C                   movel     kpjbu         trul06ds
143900070202     C*
144000070202     C                   ENDSR
144100060515     C*--------------------------------------------------------------------------------------------*
144200060515     C* *inzsr - operazioni iniziali
144300060515     C*--------------------------------------------------------------------------------------------*
144400000000     C     *inzsr        BEGSR
144500060515     C*
144600060515     C* Ricevimento parametri
144700060515     C     *ENTRY        PLIST
144800061214     C                   PARM                    KPJBA
144900061214     C*
145000061214     C* Ridefinisco i parametri
145100061214     C                   MOVEL     KPJBU         PARAM
145200061220     C*
145300061220     C* Verifico subito se richiesta stampa in dettaglio o sintetica
145400070215     C                   setoff                                       40
145500070215     C                   setoff                                       54
145600070215     C                   if        In_LIV  = 'D'
145700070215     C                   seton                                        40
145800070215     C                   endif
145900070215     C                   if        In_LIV  = 'T'
146000070215     C                   seton                                        54
146100070215     C                   endif
146200070226     C                   if        In_PIG  = 'A'
146300070226     C                   eval      In_PIG_num = 1
146400070226     C                   else
146500070226     C                   eval      In_PIG_num = 2
146600070226     C                   endif
146700070125     C*
146800070125     C* CALCOLA LA DATA CORRENTE
146900070125     C                   time                    wn14             14 0
147000070125     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
147100070125     C                   z-add     wn8           g08dat
147200070125     C                   z-add     *zeros        g08inv
147300070125     C                   movel     '0'           g08err
147400070125     C                   call      'XSRDA8'
147500070125     C                   parm                    wlbda8
147600070125     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
147700070202     C*
147800070202     C* Definizione chiavi d lettura
147900070202     C* - FNARB01L
148000070202     C     KEYarb01_c    KLIST
148100070202     C                   KFLD                    DET_AAS
148200070202     C                   KFLD                    DET_LNP
148300070202     C                   KFLD                    DET_NRS
148400070202     C                   KFLD                    DET_NSP
148500070202     C* - TABEL00F
148600070202     C     KEYtab_c      KLIST
148700070202     C                   KFLD                    KTBKUT
148800070202     C                   KFLD                    KTBCOD
148900060622     C*
149000060622     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
149100060622     C
149200060622     C/EXEC SQL
149300060622     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
149400060622     C/END-EXEC
149500060515     C*
149600060515     C                   ENDSR
