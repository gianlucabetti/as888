000100061215     H*--------------------------------------------------------------------------------------------*
000200071001     H* Verifica abbinamenti distinte/giri  RITIRI
000300061215     H*--------------------------------------------------------------------------------------------*
000400061215     H DECEDIT('0,') DATEDIT(*DMY.)
000500061215     F*--------------------------------------------------------------------------------------------*
000600061215     F* Data base
000700061215     F*--------------------------------------------------------------------------------------------*
000800071001     Ffior80p   O    E             PRINTER OFLIND(*IN05) USROPN
000900061215     D*--------------------------------------------------------------------------------------------*
001000061215     D* Data structure
001100061215     D*--------------------------------------------------------------------------------------------*
001200061215     D*------------------
001300061215     D* DS ARCHITETTURA
001400061215     D*------------------
001500061215     D KPJBA         E DS
001600061215     D*-------------------
001700061215     D* RICERCA PARAMETRI UTENTI
001800061215     D*-------------------
001900061215     D TIBS34DS      E DS                                                       *Profili utente
002000061215     D DDATIUTE      E DS                                                       *Dati utente
002100061215     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
002200070202     D*-------------------
002300070202     D* DS PER TRUL06R - CARICAMENTO £1/£6
002400070202     D*-------------------
002500070202     D TRUL06DS      E DS
002600070202     D  Lin                    1     90  0
002700070202     D                                     DIM(30)                              SKI COMODO
002800070125     D*-------------------
002900070125     D* DS PROCEDURA CONTROLLO/REPERIMENTO GIRI
003000070125     D*-------------------
003100070125     D FIDG09DS      E DS
003200061215     D*------------------
003300061215     D* DS PASSAGGIO PARAMETRI
003400061215     D*------------------
003500061215     D PARAM           DS
003600061215     D  In_FGS                        3  0
003700061215     D  In_DFV                        8  0
003800061215     D  In_NFV                        7  0
003900070201     D  In_LIV                        1
004000061220     D  In_TIP                        2
004100070223     D  In_PIG                        1
004200061215     D*------------------
004300061215     D* DS DI WRK
004400061215     D*------------------
004500071005     D DS_WOR80_DET    DS
004600061215     D  DET_FGS                       3  0
004700061215     D  DET_NPG                       1
004800061215     D  DET_NFV                       7  0
004900061215     D  DET_DFV                       8  0
005000061215     D  DET_FPP                       1
005100061215     D  DET_PDR                       7  0
005200061215     D  DET_TIP                       1
005300061215     D  DET_RSC                      35
005400061215     D  DET_CGI                      10
005500070215     D  DET_TGI                       1
005600070129     D  DET_PTY                       2  0
005700071005     D  DET_POE                       3  0
005800071005     D  DET_NSR                       2  0
005900071005     D  DET_NOR                       7  0
006000071005     D  DET_NRV                       2  0
006100071005     D  DET_POR                       3  0
006200061215     D  DET_NCL                       5  0
006300061221     D  DET_PES                       7  1
006400061221     D  DET_VOL                       5  3
006500061220     D  DET_RSD                      35
006600061220     D  DET_IND                      35
006700061220     D  DET_LOD                      35
006800061220     D  DET_CAD                      35
006900061221     D  DET_PRD                       2
007000061215     D*
007100071005     D DS_WOR80_TOTD   DS
007200061215     D  TOTD_NFV                      7  0
007300071001     D  TOTD_ORM                      9  0
007400061219     D  TOTD_NCL                      9  0
007500061221     D  TOTD_PES                      9  0
007600061221     D  TOTD_VOL                      9  2
007700061215     D*
007800071005     D DS_WOR80_TOTG   DS
007900061215     D  TOTG_NFV                      7  0
008000061215     D  TOTG_CGI                     10
008100071001     D  TOTG_ORM                      9  0
008200061219     D  TOTG_NCL                      9  0
008300061221     D  TOTG_PES                      9  0
008400061221     D  TOTG_VOL                      9  2
008500061215     D*------------------
008600061215     D* DS "XSRDA8" - CONTROLLA DATA (8)
008700061215     D*------------------
008800061215     D WLBDA8          DS
008900061215     D  G08DAT                 1      8  0
009000061215     D  G08INV                 9     16  0
009100061215     D  G08ERR                17     17
009200061215     D  G08TGI                18     22  0
009300070202     D*------------------
009400070202     D* SCHIERE D WRK
009500070202     D*------------------
009600070202     D cchco           S              2    DIM(30)
009700070202     D REC             S              2    DIM(50)
009800070202     D*------------------
009900070202     D* DS TABELLE
010000070202     D*------------------
010100070202     D DCCH          E DS
010200070202     D DS3A          E DS
010300070202     D*------------------
010400070202     D* DS controllo della presenza di CA per la spedizione richiesta
010500070202     D*------------------
010600070202     D FIDN12DS      e ds
010700070202     D  skKey                 26    305    dim(20)
010800070202     D  skAnn                306    325    dim(20)
010900070202     D  skDca                326    485  0 dim(20)
011000070202     D  skFca                486    545  0 dim(20)
011100070202     D  skDch                546    705  0 dim(20)
011200070202     D  skCch                706    745    dim(20)
011300070202     D*
011400070202     D dsKey           ds
011500070202     D  dsaac                         4  0
011600070202     D  dsfil                         3  0
011700070202     D  dsnca                         7  0
011800061215     D*------------------
011900061215     D* VARIABILI D WRK
012000061215     D*------------------
012100061215     D FGS_DA          S              3  0 inz(*loval)
012200061215     D FGS_A           S              3  0 inz(*hival)
012300061215     D DFV_DA          S              8  0 inz(*loval)
012400061215     D DFV_A           S              8  0 inz(*hival)
012500061215     D NFV_DA          S              7  0 inz(*loval)
012600061215     D NFV_A           S              7  0 inz(*hival)
012700061222     D SAV_NFV         S                   like(DET_NFV) inz
012800061222     D SAV_CGI         S                   like(DET_CGI) inz
012900070215     D SAV_PTY         S                   like(DET_PTY) inz
012901071108     D SAV_NSR         S                   like(DET_NSR) inz
013000061222     D In_DFV_stp      S                   like(In_DFV)  inz
013100070226     D In_PIG_num      S              1  0 inz
013200071001     D wTotORM         S              7  0 inz(*zeros)
013300061215     C*--------------------------------------------------------------------------------------------*
013400061215     C* Main lines
013500061215     C*--------------------------------------------------------------------------------------------*
013501071106     C*
013502071106     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013503071106     C
013504071106     C/EXEC SQL
013505071106     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013506071106     C/END-EXEC
013600061215     C*
013700061215     C* Reperimento dati utente
013800061215     C                   exsr      repdatiute
013900070202     C*
014000070202     C* Caricamento P.O. in £6
014100070202     C                   exsr      carlira
014200070202     C*
014300061215     C*
014400061215     C* Verifica parametri d lancio
014500061215     C                   exsr      chkpar
014600061215     C*
014700061215     C* Deleto la tabella temporanea d wrk
014800061215     C                   exsr      dlttab
014900061215     C*
015000061215     C* Creo la tabella temporanea d wrk
015100061215     C                   exsr      crttab
015200061215     C*
015300061214     C* Reperisco i dati e popolo la tabella temporanea
015400070205     C                   if        In_DFV   = *zeros AND
015500070205     C                             In_NFV   = *zeros
015600070205     C                   exsr      repdatinodfv
015700070205     C                   else
015800070205     C                   exsr      repdatisidfv
015900070205     C                   endif
016000070202     C*
016100071001     C* Se lancio x ORM   nn in distinta => eseguo scrematura ORM
016200071001     C                   exsr      exclorm
016300061214     C*
016400061220     C* A seconda del tipo lancio eseguo:
016500061214     C* - distinta/giro
016600070215     C                   if        In_TIP  = 'DG' and
016700070215     C                             In_LIV <> 'T'
016800061214     C                   exsr      sta_ndc_cgi
016900061220     C                   endif
017000070215     C* - distinta/priorità
017100070215     C                   if        In_TIP = 'DG' and
017200070215     C                             In_LIV = 'T'
017300070215     C                   exsr      sta_ndc_pty
017400061220     C                   endif
017500061215     C* Termine programma
017600061215     C                   seton                                        lr
017700060515     C*
017800061215     C*------------------------------------------------------------------------*
017900061215     C* CHKPAR - Verifica parametri d lancio
018000061215     C*------------------------------------------------------------------------*
018100061215     C     CHKPAR        BEGSR
018200061215     C*
018300061215     C* P.O. gestione
018400070202     C****               eval      FGS_DA = In_FGS
018500070202     C****               eval      FGS_A  = In_FGS
018600061215     C*
018700061215     C* Data distinta
018800061215     C                   if        In_DFV <> *zeros
018900061215     C                   eval      DFV_DA = In_DFV
019000061215     C                   eval      DFV_A  = In_DFV
019100061222     C* - giro la data distinta
019200061222     C                   z-add     In_DFV        G08INV
019300061222     C                   z-add     *ZEROS        G08DAT
019400061222     C                   movel     '3'           G08ERR
019500061222     C                   call      'XSRDA8'
019600061222     C                   parm                    WLBDA8
019700061222     C                   z-add     G08DAT        In_DFV_stp
019800061215     C                   endif
019900061215     C*
020000061215     C* Numero distinta
020100061215     C                   if        In_NFV <> *zeros
020200061215     C                   eval      NFV_DA = In_NFV
020300061215     C                   eval      NFV_A  = In_NFV
020400061215     C                   endif
020500070202     C*
020600071001     C* Se lancio x orm   NN in distinta verifico consegnabilità d ogni orm
020700070202     C                   if        In_DFV   = *zeros AND
020800070202     C                             In_NFV   = *zeros
020900070202     C                   eval      DFV_DA = *loval
021000070202     C                   eval      DFV_A  = *hival
021100070205     C                   eval      NFV_DA = *zeros
021200070205     C                   eval      NFV_A  = *zeros
021300070202     C                   endif
021400061215     C*
021500061215     C                   ENDSR
021600061215     C*------------------------------------------------------------------------*
021700061215     C* DLTTAB - Routine di cancellazione tabella temporanea
021800061215     C*------------------------------------------------------------------------*
021900061215     C     DLTTAB        BEGSR
022000061215     C*
022100061215     C/EXEC SQL
022200071001     C+ DROP TABLE QTEMP/WFOR800F
022300061215     C/END-EXEC
022400061215     C*
022500061215     C                   ENDSR
022600061214     C*------------------------------------------------------------------------*
022700061214     C* CRTTAB - Routine di creazione tabella temporanea
022800061214     C*------------------------------------------------------------------------*
022900061214     C     CRTTAB        BEGSR
023000061214     C*
023100061214     C/EXEC SQL
023200071001     C+ CREATE TABLE QTEMP/WFOR800F (
023300071001     C+ WOR80FGS DEC (3, 0) NOT NULL WITH DEFAULT,
023400071001     C+ WOR80NPG CHAR (1 ) NOT NULL WITH DEFAULT,
023500071001     C+ WOR80NFV DEC (7, 0) NOT NULL WITH DEFAULT,
023600071001     C+ WOR80DFV DEC (8, 0) NOT NULL WITH DEFAULT,
023700071001     C+ WOR80FPP CHAR (1 ) NOT NULL WITH DEFAULT,
023800071001     C+ WOR80PDR DEC (7, 0) NOT NULL WITH DEFAULT,
023900071001     C+ WOR80TIP CHAR (1 ) NOT NULL WITH DEFAULT,
024000071001     C+ WOR80RSC CHAR (35 ) NOT NULL WITH DEFAULT,
024100071001     C+ WOR80CGI CHAR (10 ) NOT NULL WITH DEFAULT,
024200071001     C+ WOR80TGI CHAR (1 ) NOT NULL WITH DEFAULT,
024300071001     C+ WOR80PTY DEC (2, 0) NOT NULL WITH DEFAULT,
024400071001     C+ WOR80POE DEC (3, 0) NOT NULL WITH DEFAULT,
024500071001     C+ WOR80NSR DEC (2, 0) NOT NULL WITH DEFAULT,
024600071001     C+ WOR80NOR DEC (7, 0) NOT NULL WITH DEFAULT,
024700071001     C+ WOR80NRV DEC (2, 0) NOT NULL WITH DEFAULT,
024800071001     C+ WOR80POR DEC (3, 0) NOT NULL WITH DEFAULT,
024900071001     C+ WOR80NCL DEC (5, 0) NOT NULL WITH DEFAULT,
025000071001     C+ WOR80PES DEC (7, 1) NOT NULL WITH DEFAULT,
025100071001     C+ WOR80VOL DEC (5, 3) NOT NULL WITH DEFAULT,
025200071001     C+ WOR80RSM CHAR (35 ) NOT NULL WITH DEFAULT,
025300071001     C+ WOR80IND CHAR (35 ) NOT NULL WITH DEFAULT,
025400071001     C+ WOR80LOM CHAR (35 ) NOT NULL WITH DEFAULT,
025500071001     C+ WOR80CAM CHAR (9 ) NOT NULL WITH DEFAULT,
025600071005     C+ WOR80PRM CHAR (2 ) NOT NULL WITH DEFAULT)
025700061214     C/END-EXEC
025800061214     C*
025900061214     C                   ENDSR
026000061214     C*------------------------------------------------------------------------*
026100070205     C* REPDATISIDFV - Routine di reperimento dati rispetto alle selezioni a video - si distinta
026200061214     C*------------------------------------------------------------------------*
026300070202     C     REPDATISIDFV  BEGSR
026400061214     C*
026500061214     C/EXEC SQL
026600071005     C+ INSERT INTO QTEMP/WFOR800F SELECT dstfgs, dstnpg, dstnfv, dstdfv,
026700071008     C+ dstfpp, dstpdr, apdtip, apdrsc, orgcgi, orgtgi, getcgipto(apdtip,
026800071005     C+ dstpdr, dstfpp, dstfgs, orgcgi, dstdfv, dstnfv, :In_PIG_num),
026900071005     C+ ormpoe, ormnsr, ormnor, ormnrv, ormpor, ormncl, orgpkg, orgvlm,
027000071005     C+ ormrsr, orminr, ormlor, ormcar, ormprr FROM fnorg00f join
027100071005     C+ fnorm00f on ormpoe = orgpoe and ormnsr = orgnsr and ormnor =
027200071005     C+ orgnor and ormnrv = orgnrv join fidst00f on ormpor = dstfgs and
027300071005     C+ orgndc = dstnfv and orgddc = dstdfv join fiapd00f on
027400071005     C+ dstpdr=apdpdr and dsttipa=apdtip WHERE orgddc >= :DFV_DA and
027500071005     C+ orgddc <= :dfv_a and ormpor = :IN_FGS and orgndc >= :nfv_da and
027600071005     C+ orgndc <= :nfv_a and dstnpg='4' and dstfcf=' ' and dstdfv >=
027700071005     C+ :DFV_DA and dstdfv <= :DFV_A and dstnfv >= :NFV_DA and dstnfv <=
027800071005     C+ :NFV_A and dsttipa = 'A'
027900061214     C/END-EXEC
028000061214     C*
028100061214     C                   ENDSR
028200070202     C*------------------------------------------------------------------------*
028300070205     C* REPDATINODFV - Routine di reperimento dati rispetto alle selezioni a video - no distinta
028400070202     C*------------------------------------------------------------------------*
028500070202     C     REPDATINODFV  BEGSR
028600070202     C*
028700070205     C/EXEC SQL
028800071005     C+ insert into qtemp/wfor800f
028900071005     C+ select :In_FGS, ' ', 0, 0, ' ', 0, ' ', ' ', orgcgi, orgtgi, 0,
029000071005     C+ ormpoe, ormnsr, ormnor, ormnrv, ormpor,
029100071005     C+ ormncl, orgpkg, orgvlm,
029200071005     C+ ormrsr, orminr, ormlor, ormcar, ormprr
029300071005     C+ from filtra201/fnorm00f join filtra201/fnorg00f
029400071005     C+ on ormpoe = orgpoe and ormnsr = orgnsr and ormnor =
029500071005     C+ orgnor and ormnrv = orgnrv
029600071005     C+ where ormndc=0
029700070205     C/END-EXEC
029800070202     C*
029900070202     C                   ENDSR
030000070202     C*------------------------------------------------------------------------*
030100071005     C* EXCLORM  - Routine di esclusione ORM
030200070202     C*------------------------------------------------------------------------*
030300071005     C     EXCLORM       BEGSR
030400070205     C*
030500070202     C/EXEC SQL
030600070202     C+ declare C9 cursor for
030700071005     C+ select * from wfor800f
030800070202     C/END-EXEC
030900070202     C
031000070202     C/EXEC SQL
031100070202     C+ open C9
031200070202     C/END-EXEC
031300070202     C
031400070202     C/EXEC SQL
031500071005     C+ Fetch next from C9 into :DS_WOR80_DET
031600070202     C/END-EXEC
031700070202     C
031800070202     C*
031900070202     C* Se reperito qualcosa => analizzo
032000070202     C                   dow       sqlcod = *zeros
032100070205     C*
032200071005     C                   movel     'N'           EscORM            1
032300070202     C*
032400071005     C* Escludo ORM   nn in "lira" del P.O. richiesto
032500071005     C     DET_POR       lookup    Lin                                    99
032600070202     C                   if        not *IN99 = *on
032700071005     C                   eval      EscORM  ='S'
032800070202     C                   endif
032900070202     C*
033000071005     C* Se ORM   da escludere => elimino il record corrente dalla tabella sql
033100071005     C                   if        EscORM   = 'S'
033200070202     C/EXEC SQL
033300071005     C+ delete from wfOR800f where CURRENT OF C9
033400070202     C/END-EXEC
033500071109     C                   else
033501071109     C*
033502071109     C* Aggiorno la tabella nel campo delle PTY se ORM con serie
033503071109     C                   If        det_nsr <> *zeros
033510071109     C/EXEC SQL
033511071109     C+ update  wfOR800f set det_pty = 'RC'
033512071109     C+ where CURRENT OF C9
033513071109     C/END-EXEC
033514071109     C                   endif
033600070202     C*
033601071109     C                   endif
033602071109     C*
033700070202     C* Proseguo con la lettura della tabella sql
033800070202     C/EXEC SQL
033900071005     C+ Fetch next from C9 into :DS_WOR80_DET
034000070202     C/END-EXEC
034100070202     C*
034200070202     C                   enddo
034300070202     C*
034400070202     C/EXEC SQL
034500070202     C+ close C9
034600070202     C/END-EXEC
034700070202     C*
034800070202     C                   ENDSR
034900060516     C*------------------------------------------------------------------------*
035000071005     C* STA_NDC_CGI - Routine di stampa x distita/giro RITIRO
035100060516     C*------------------------------------------------------------------------*
035200061214     C     STA_NDC_CGI   BEGSR
035300061215     C*
035400061215     C* Apertura printer-file
035500071005     C                   open      FIOR80P
035600061220     C*
035700061220     C* Valorizzo titolo tabulato
035800071008     C                   eval      stttes1 = '*** VERIFICA ABBINAMENTI ' +
035900071008     C                                       'DISTINTE/GIRI RITIRI ***'
036000070201     C                   if        In_LIV = 'S'
036100070125     C                   eval      stttes2 = '(SINTETICA)'
036200070117     C                   else
036300070201     C                   eval      stttes2 = '(DETTAGLIO)'
036400070201     C                   endif
036500061222     C*
036600061222     C* Valorizzo i paramteri d lancio
036700070201     C                   eval      stppar = 'P.O.: ' +
036800070201     C                                       %trim(%editw(In_FGS:'0   '))
036900061222     C                   if        In_NFV <> *zeros
037000070116     C                   eval      stppar = %trim(stppar) + ' '+
037100071005     C                                      'PARAMETRI: tutti gli ORM  nella ' +
037200061222     C                                      'distinta '                        +
037300061222     C                                       %trim(%editw(In_FGS:'0   '))      +
037400061222     C                                       ' '                               +
037500061222     C                                       %trim(%editc(In_NFV:'4'))
037600061222     C                   else
037700070201     C                   if        In_DFV <> *zeros
037800070116     C                   eval      stppar = %trim(stppar) + ' '+
037900071005     C                                      'PARAMETRI: tutti gli ORM  nelle ' +
038000061222     C                                      'distinte in data '                +
038100061222     C                                      %editw(In_DFV_stp:'0  /  /    ')
038200070201     C                   else
038300070201     C                   eval      stppar = %trim(stppar) + ' '+
038400071005     C                                      'PARAMETRI: tutti gli ORM  non ' +
038500070201     C                                      'inserite in nessuna distinta'
038600070201     C                   endif
038700061222     C                   endif
038800061218     C*
038900061218     C* Se scelta stampa nn dettaglio stampo subito la testata
039000061220     C                   if        *in40 = *off
039100071005     C                   write     or80te
039200061222     C                   seton                                        60
039300071005     C                   write     or80dte
039400071005     C                   write     or80ri
039500061222     C                   setoff                                       05
039600061218     C                   endif
039700060516     C*
039800060516     C/EXEC SQL
039900061215     C+ declare C1 cursor for
040000071005     C+ select * from wfor800f
040100071005     C+ order by wor80nfv, wor80pty, wor80cgi,
040101071106     C+ wor80nsr,
040200071008     C+ wor80prm, wor80lom, wor80ind, wor80cam, wor80rsm
040300061214     C+ for read only
040400060516     C/END-EXEC
040500060516     C
040600060516     C/EXEC SQL
040700061215     C+ open C1
040800060516     C/END-EXEC
040900060516     C
041000060516     C/EXEC SQL
041100071005     C+ Fetch next from C1 into :DS_WOR80_DET
041200060516     C/END-EXEC
041300060516     C*
041400060516     C* Se reperito qualcosa => analizzo
041500061214     C                   dow       sqlcod = *zeros
041600061220     C*
041700061220     C* Valorizzo i dati in stampa relativi alla distinta
041800061220     C*
041900061220     C* - codice distinta
042000061220     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
042100061220     C                                       ' ' +
042200061220     C                                       %trim(%editc(det_nfv:'4'))
042300061220     C* - autotrasportatore
042400061220     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
042500061220     C                                       ' ' +
042600061220     C                                       det_rsc
042700061220     C* - data distinta
042800061220     C                   z-add     det_dfv       G08INV
042900061220     C                   z-add     *ZEROS        G08DAT
043000061220     C                   movel     '3'           G08ERR
043100061220     C                   call      'XSRDA8'
043200061220     C                   parm                    WLBDA8
043300061220     C                   z-add     G08DAT        stpdat
043400061220     C* - prestazione distinta
043500061220     C                   if        det_fpp = 'M'
043600061220     C                   eval      stpprest = 'MAT.'
043700061220     C                   endif
043800061220     C                   if        det_fpp = 'P'
043900061220     C                   eval      stpprest = 'POM.'
044000061220     C                   endif
044100061218     C*
044200061218     C* Se scelta stampa anche dettaglio ad ogni distinta salto pagina
044300061220     C                   if        *in40 = *on
044400061220     C                   eval      stpdes = 'Distinta: '+
044500061220     C                                %trim(stpdist)+' del '+
044600061220     C                                %editw(stpdat:'0  /  /    ')+' '+stpprest+
044700061220     C                                '    '+'Autotrasportatore: '+
044800061220     C                                stpaut
044900071005     C   60              write     or80ri
045000071005     C   60              write     or80er
045100071005     C                   write     or80te
045200061222     C                   seton                                        60
045300061222     C                   setoff                                       05
045400061218     C                   endif
045500061218     C*
045600061220     C* Inizializzo l'indicatore d nuova distinta
045700061218     C                   setoff                                       51
045800061215     C*
045900061215     C* Salvo la distinta corrente
046000061215     C                   eval      SAV_NFV = DET_NFV
046100061215     C*
046200061215     C/EXEC SQL
046300061215     C+ declare C2 cursor for
046400071005     C+ select wor80nfv, count(*), decimal(sum(wor80ncl), 9, 0),
046500071005     C+ decimal(sum(wor80pes), 9, 0), decimal(sum(wor80vol), 9, 2)
046600071005     C+ from wfor800f
046700071005     C+ where wor80nfv = :SAV_NFV
046800071005     C+ group by wor80nfv
046900061215     C+ for read only
047000061215     C/END-EXEC
047100061215     C
047200061215     C/EXEC SQL
047300061215     C+ open C2
047400061215     C/END-EXEC
047500061215     C
047600061215     C/EXEC SQL
047700071005     C+ Fetch next from C2 into :DS_WOR80_TOTD
047800061215     C/END-EXEC
047900061215     C
048000061215     C/EXEC SQL
048100061215     C+ close C2
048200061215     C/END-EXEC
048300061215     C*
048400071005     C* Ad ogni rottura d distinta reperisco il totale orm   in distinta
048500061215     C                   dow       sqlcod = *zeros AND
048600061215     C                             DET_NFV = SAV_NFV
048700061215     C*
048800061215     C* Salvo il giro corrente
048900061215     C                   eval      SAV_CGI = DET_CGI
048901071108     C*
048902071108     C* Salvo la serie
048903071108     C                   eval      SAV_NSR = DET_NSR
049000061215     C*
049100061215     C/EXEC SQL
049200061215     C+ declare C3 cursor for
049300071107     C+ select wor80nfv, 'CONTESTUAL' as wor80cgi, count(*),
049400071005     C+ decimal(sum(wor80ncl), 9, 0),
049500071005     C+ decimal(sum(wor80pes), 9, 0),
049600071005     C+ decimal(sum(wor80vol)+0.005, 9, 2)
049700071005     C+ from wfor800f
049800071005     C+ where wor80nfv = :SAV_NFV and wor80cgi = :SAV_CGI
049801071106     C+   and wor80nsr<>0         and :SAV_CGI = ' '
049802071107     C+ group by wor80nfv, wor80cgi
049803071106     C+ UNION
049804071106     C+ select wor80nfv, wor80cgi, count(*),
049805071106     C+ decimal(sum(wor80ncl), 9, 0),
049806071106     C+ decimal(sum(wor80pes), 9, 0),
049807071106     C+ decimal(sum(wor80vol)+0.005, 9, 2)
049808071106     C+ from wfor800f
049809071106     C+ where wor80nfv = :SAV_NFV and wor80cgi = :SAV_CGI
049810071106     C+   and wor80nsr=0          and :SAV_CGI = ' '
049811071107     C+ group by wor80nfv, wor80cgi
049812071106     C+ UNION
049813071106     C+ select wor80nfv, wor80cgi, count(*),
049814071106     C+ decimal(sum(wor80ncl), 9, 0),
049815071106     C+ decimal(sum(wor80pes), 9, 0),
049816071106     C+ decimal(sum(wor80vol)+0.005, 9, 2)
049817071106     C+ from wfor800f
049818071106     C+ where wor80nfv = :SAV_NFV and wor80cgi = :SAV_CGI
049819071106     C+                           and :SAV_CGI <>' '
049900071005     C+ group by wor80nfv, wor80cgi
050000061215     C+ for read only
050100061215     C/END-EXEC
050200061215     C
050300061215     C/EXEC SQL
050400061215     C+ open C3
050500061215     C/END-EXEC
050600061215     C
050700061215     C/EXEC SQL
050800071005     C+ Fetch next from C3 into :DS_Wor80_TOTG
050900061215     C/END-EXEC
051000061215     C
051100061215     C/EXEC SQL
051200061215     C+ close C3
051300061215     C/END-EXEC
051400061220     C*
051500061220     C* Valorizzo i dati in stampa relativi al giro
051600070202     C                   clear                   stpcgipty
051700061215     C                   eval      stpgiro   = det_cgi
051800070202     C* - decodifica giro e priorità
051900070125     C                   if        det_cgi <> *blanks
052000070215     C                   eval      stpcgipty = %editc(det_pty:'4')
052100070125     C                   clear                   fidg09ds
052200070125     C                   eval      d09iop0 = '001'
052300070202     C                   eval      d09ifgs = det_fgs
052400070125     C                   eval      d09idat = datcor
052500070125     C                   eval      d09icgi = det_cgi
052600071005     C                   eval      d09itug = 'R'
052700070125     C                   movel(p)  fidg09ds      KPJBU
052800070125     C                   call      'FIDG09R'
052900070125     C                   parm                    KPJBA
053000070125     C                   movel(p)  KPJBU         fidg09ds
053100070125     C                   if        d09oerr = *blanks
053200070205     C                   eval      stpain   = d09oain
053300070125     C                   eval      stpgirod = d09odes
053400070125     C                   else
053500070205     C                   eval      stpain   = *all'*'
053600070125     C                   eval      stpgirod = *all'*'
053700070125     C                   endif
053800070125     C*
053900070125     C                   else
054000070215     C                   eval      stpain    = *blanks
054001071107      * verifico se contestuale
054002071107     c                   If        DET_nsr = *zeros
054100070215     C                   eval      stpgirod  = '*** non attribuito ***'
054200070215     C                   eval      stpcgipty = '--'
054201071107     c                   else
054202071107     C                   eval      stpgirod  = '** Ritiri contestuali **'
054203071107     C                   eval      stpcgipty = 'RC'
054204071107     c                   endif
054300070125     C                   endif
054400070202     C*
054500070202     C                   exsr      valdet
054600061218     C*
054700061218     C* Gestisco salto pagina
054800061218     C                   if        *in05 = *on
054900071005     C   60              write     or80ri
055000071005     C   60              write     or80er
055100071005     C                   write     or80te
055200061222     C                   seton                                        60
055300071005     C  N40              write     or80dte
055400071005     C  N40              write     or80ri
055500061218     C                   setoff                                       05
055600061218     C*
055700061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
055800061220     C                   if        *in40 = *on
055900071005     C                   write     or80ri
056000071005     C                   write     or80dtd
056100071005     C                   write     or80ri
056200071005     C                   write     or80gtd
056300071005     C                   write     or80ri
056400061218     C                   endif
056500061218     C                   endif
056600061219     C*
056700061219     C* Se richiesta stampa in dettaglio lascio 1 riga vuota ad ogni cambio giro
056800061220     C                   if        *in40 = *on
056900071005     C   51              write     or80sp
057000061219     C                   endif
057100061218     C*
057200061218     C* Quindi stampo riga totali x distinta/giro
057300070202     C                   if        *in40 = *off
057400070202     C                   if        *in51 = *off
057500071005     C                   write     or80dt
057600070202     C                   seton                                        51
057700070202     C                   exsr      valdet
057800071005     C                   write     or80dtg
057900070202     C                   else
058000071005     C                   write     or80dtg
058100070202     C                   endif
058200070202     C                   endif
058300070202     C*
058400071005     C   40              write     or80ri
058500071005     C   40              write     or80dtd
058600071005     C   40              write     or80ri
058700071005     C   40              write     or80gtd
058800071005     C   40              write     or80ri
058900061218     C                   seton                                        51
059000061215     C*
059100071005     C* Ad ogni rottura d giro reperisco i totali orm  , colli. peso, volume x distinta/giro
059200061219     C                   dow       sqlcod = *zeros   AND
059300061219     C                             DET_NFV = SAV_NFV AND
059400071108     C                             DET_CGI = SAV_CGI and
059402071108     c                             (Sav_nsr = det_nsr or (sav_nsr <> det_nsr and
059403071108     c                             sav_nsr <> *zeros))
059500071108     C*
059600061215     C* Se scelta stampa anche dettaglio => provvedo
059700061220     C                   if        *in40 = *on
059800071005     C                   eval      stdorm  = %editw(det_poe:'0   ') +
059900061220     C                                       ' ' +
060000071005     C                                       %trim(%editc(det_nsr:'4')) +
060100061220     C                                       ' ' +
060200071005     C                                       %editc(det_nor:'4') +
060300061220     C                                       ' ' +
060400071005     C                                       %editc(det_nrv:'4')
060500070215     C                   if        det_tgi = 'M'
060600070215     C                   eval      stdtgi = 'M'
060700070215     C                   else
060800070215     C                   eval      stdtgi = *blanks
060900070215     C                   endif
061000071005     C                   eval      stdrsm = det_rsd
061100061220     C                   eval      stdind = det_ind
061200061220     C                   eval      stdloc = det_lod
061300061220     C                   eval      stdcap = det_cad
061400061221     C                   eval      stdprd = det_prd
061500061220     C***                eval      stdznc = det_znc
061600071005     C***                eval      stdncl =
061700071005     C****                                   %subst(%editc(det_ncl:'2'):1:6)
061800070125     C                   eval      stdpkf =
061900061221     C                                       %subst(%editc(det_pes:'2'):3:7)
062000070125     C                   eval      stdvlf = %editc(det_vol:'2')
062100061222     C*
062200061218     C*
062300061218     C* Gestisco salto pagina
062400061218     C                   if        *in05 = *on
062500071005     C   60              write     or80ri
062600071005     C   60              write     or80er
062700071005     C                   write     or80te
062800061222     C                   seton                                        60
062900071005     C  N40              write     or80dte
063000071005     C  N40              write     or80ri
063100061218     C                   setoff                                       05
063200061218     C*
063300061218     C* Se scelta stampa anche dettaglio => stampo anche descrizione dettaglio
063400061220     C                   if        *in40 = *on
063500071005     C                   write     or80ri
063600071005     C                   write     or80dtd
063700071005     C                   write     or80ri
063800071005     C                   write     or80gtd
063900071005     C                   write     or80ri
064000061218     C                   endif
064100061218     C                   endif
064200061215     C*
064300071005     C* Stampo riga d dettaglio orm
064400071005     C                   write     or80dd
064500061215     C                   endif
064600060516     C*
064700061214     C* Proseguo con la lettura del recordset
064800061214     C/EXEC SQL
064900071005     C+ Fetch next from C1 into :DS_WOR80_DET
065000061214     C/END-EXEC
065100061214     C                   enddo
065200061215     C*
065300061215     C                   enddo
065400070117     C*
065500071005     C* Totalizzo gli ORM
065600071005     C                   eval      wTotORM   = wTotORM   + totd_orm
065700061215     C*
065800061215     C                   enddo
065900060516     C*
066000060516     C/EXEC SQL
066100061215     C+ close C1
066200060516     C/END-EXEC
066300070117     C*
066400071005     C                   evalr     stototorm = %editc(wTotorm  :'2')
066500061215     C*
066600061215     C* Chiusura printer-file
066700071005     C                   write     or80ri
066800071005     C                   write     or80tt
066900071005     C                   write     or80ri
067000071005     C                   write     or80er
067100071005     C                   write     or80bk
067200071005     C                   write     or80bk
067300071005     C                   write     or80fi
067400071005     C                   close     FIOR80P
067500060516     C*
067600060516     C                   ENDSR
067700070215     C*------------------------------------------------------------------------*
067800070215     C* STA_NDC_PTY - Routine di stampa x distita/priorità (solo totali)
067900070215     C*------------------------------------------------------------------------*
068000070215     C     STA_NDC_PTY   BEGSR
068100070215     C*
068200070215     C* Apertura printer-file
068300071005     C                   open      FIOR80P
068400070215     C*
068500070215     C* Valorizzo titolo tabulato
068600071008     C                   eval      stttes1 = '* VERIFICA ABBINAMENTI ' +
068700071008     C                                       'DISTINTE/PRIORITA RITIRI *'
068800070215     C                   eval      stttes2 = ' (TOTALI) '
068900070215     C*
069000070215     C* Valorizzo i paramteri d lancio
069100070215     C                   eval      stppar = 'P.O.: ' +
069200070215     C                                       %trim(%editw(In_FGS:'0   '))
069300070215     C                   if        In_NFV <> *zeros
069400070215     C                   eval      stppar = %trim(stppar) + ' '+
069500071005     C                                      'PARAMETRI: tutti gli ORM  nella ' +
069600070215     C                                      'distinta '                        +
069700070215     C                                       %trim(%editw(In_FGS:'0   '))      +
069800070215     C                                       ' '                               +
069900070215     C                                       %trim(%editc(In_NFV:'4'))
070000070215     C                   else
070100070215     C                   if        In_DFV <> *zeros
070200070215     C                   eval      stppar = %trim(stppar) + ' '+
070300071005     C                                      'PARAMETRI: tutti gli ORM  nelle ' +
070400070215     C                                      'distinte in data '                +
070500070215     C                                      %editw(In_DFV_stp:'0  /  /    ')
070600070215     C                   else
070700070215     C                   eval      stppar = %trim(stppar) + ' '+
070800071005     C                                      'PARAMETRI: tutti gli ORM  non ' +
070900070215     C                                      'inserite in nessuna distinta'
071000070215     C                   endif
071100070215     C                   endif
071200070215     C*
071300070215     C* Se scelta stampa nn dettaglio stampo subito la testata
071400070215     C                   if        *in40 = *off
071500071005     C                   write     or80te
071600070215     C                   seton                                        60
071700071005     C                   write     or80dte
071800071005     C                   write     or80ri
071900070215     C                   setoff                                       05
072000070215     C                   endif
072100070215     C*
072200070215     C/EXEC SQL
072300070215     C+ declare C10 cursor for
072400071005     C+ select * from wfor800f
072500071005     C+ order by wor80nfv, wor80pty
072600070215     C+ for read only
072700070215     C/END-EXEC
072800070215     C
072900070215     C/EXEC SQL
073000070215     C+ open C10
073100070215     C/END-EXEC
073200070215     C
073300070215     C/EXEC SQL
073400071005     C+ Fetch next from C10 into :DS_WOR80_DET
073500070215     C/END-EXEC
073600070215     C*
073700070215     C* Se reperito qualcosa => analizzo
073800070215     C                   dow       sqlcod = *zeros
073900070215     C*
074000070215     C* Valorizzo i dati in stampa relativi alla distinta
074100070215     C*
074200070215     C* - codice distinta
074300070215     C                   eval      stpdist = %trim(%editw(det_fgs:'0   ')) +
074400070215     C                                       ' ' +
074500070215     C                                       %trim(%editc(det_nfv:'4'))
074600070215     C* - autotrasportatore
074700070215     C                   eval      stpaut  = %trim(%editc(det_pdr:'4')) +
074800070215     C                                       ' ' +
074900070215     C                                       det_rsc
075000070215     C* - data distinta
075100070215     C                   z-add     det_dfv       G08INV
075200070215     C                   z-add     *ZEROS        G08DAT
075300070215     C                   movel     '3'           G08ERR
075400070215     C                   call      'XSRDA8'
075500070215     C                   parm                    WLBDA8
075600070215     C                   z-add     G08DAT        stpdat
075700070215     C* - prestazione distinta
075800070215     C                   if        det_fpp = 'M'
075900070215     C                   eval      stpprest = 'MAT.'
076000070215     C                   endif
076100070215     C                   if        det_fpp = 'P'
076200070215     C                   eval      stpprest = 'POM.'
076300070215     C                   endif
076400070215     C*
076500070215     C* Inizializzo l'indicatore d nuova distinta
076600070215     C                   setoff                                       51
076700070215     C*
076800070215     C* Salvo la distinta corrente
076900070215     C                   eval      SAV_NFV = DET_NFV
077000070215     C*
077100070215     C/EXEC SQL
077200070215     C+ declare C11 cursor for
077300071005     C+ select wor80nfv, count(*),
077400071005     C+ decimal(sum(wor80ncl), 9, 0),
077500071005     C+ decimal(sum(wor80pes), 9, 0),
077600071005     C+ decimal(sum(wor80vol), 9, 2)
077700071005     C+ from wfor800f
077800071005     C+ where wor80nfv = :SAV_NFV
077900071005     C+ group by wor80nfv
078000070215     C+ for read only
078100070215     C/END-EXEC
078200070215     C
078300070215     C/EXEC SQL
078400070215     C+ open C11
078500070215     C/END-EXEC
078600070215     C
078700070215     C/EXEC SQL
078800071005     C+ Fetch next from C11 into :DS_Wor80_TOTD
078900070215     C/END-EXEC
079000070215     C
079100070215     C/EXEC SQL
079200070215     C+ close C11
079300070215     C/END-EXEC
079400070215     C*
079500071005     C* Ad ogni rottura d distinta reperisco il totale orm   in distinta
079600070215     C                   dow       sqlcod = *zeros AND
079700070215     C                             DET_NFV = SAV_NFV
079800070215     C*
079900070215     C* Salvo il giro corrente
080000070215     C                   eval      SAV_PTY = DET_PTY
080100070215     C*
080200070215     C/EXEC SQL
080300070215     C+ declare C12 cursor for
080400071005     C+ select wor80nfv, wor80pty, count(*),
080500071005     C+ decimal(sum(wor80ncl), 9, 0),
080600071005     C+ decimal(sum(wor80pes), 9, 0),
080700071005     C+ decimal(sum(wor80vol)+0.005, 9, 2)
080800071005     C+ from wfor800f
080900071005     C+ where wor80nfv = :SAV_NFV and wor80pty = :SAV_PTY
081000071005     C+ group by wor80nfv, wor80pty
081100070215     C+ for read only
081200070215     C/END-EXEC
081300070215     C
081400070215     C/EXEC SQL
081500070215     C+ open C12
081600070215     C/END-EXEC
081700070215     C
081800070215     C/EXEC SQL
081900071005     C+ Fetch next from C12 into :DS_Wor80_TOTG
082000070215     C/END-EXEC
082100070215     C
082200070215     C/EXEC SQL
082300070215     C+ close C12
082400070215     C/END-EXEC
082500070215     C*
082600070215     C* Valorizzo i dati in stampa relativi al giro
082700070215     C                   clear                   stpcgipty
082800070215     C                   eval      stpgiro   = *blanks
082900070215     C* - decodifica giro e priorità
083000070215     C                   if        det_cgi <> *blanks
083100070215     C                   eval      stpcgipty = %editc(det_pty:'4')
083200070215     C                   else
083201071107      * se orm senza serie
083202071107     c                   if        det_nsr = *zeros
083300070215     C                   eval      stpcgipty = '--'
083301071107     c                   else
083302071107      * orm con serie ritiro contestuale
083303071107     C                   eval      stpcgipty = 'RC'
083304071107     c                   endif
083400070215     C                   endif
083500070215     C*
083600070215     C                   exsr      valdet
083700070215     C*
083800070215     C* Gestisco salto pagina
083900070215     C                   if        *in05 = *on
084000071005     C   60              write     or80ri
084100071005     C   60              write     or80er
084200071005     C                   write     or80te
084300070215     C                   seton                                        60
084400071005     C  N40              write     or80dte
084500071005     C  N40              write     or80ri
084600070215     C                   setoff                                       05
084700070215     C                   endif
084800070215     C*
084900070215     C* Quindi stampo riga totali x distinta/priorità
085000070215     C                   if        *in51 = *off
085100071005     C                   write     or80dt
085200070215     C                   seton                                        51
085300070215     C                   exsr      valdet
085400071005     C                   write     or80dtg
085500070215     C                   else
085600071005     C                   write     or80dtg
085700070215     C                   endif
085800070215     C                   seton                                        51
085900070215     C*
086000071005     C* Ad ogni rottura d giro reperisco i totali orm  , colli. peso, volume x distinta/giro
086100070215     C                   dow       sqlcod = *zeros   AND
086200070215     C                             DET_NFV = SAV_NFV AND
086300070215     C                             DET_PTY = SAV_PTY
086400070215     C*
086500070215     C* Proseguo con la lettura del recordset
086600070215     C/EXEC SQL
086700071005     C+ Fetch next from C10 into :DS_Wor80_DET
086800070215     C/END-EXEC
086900070215     C                   enddo
087000070215     C*
087100070215     C                   enddo
087200070215     C*
087300071005     C* Totalizzo gli ORM
087400071005     C                   eval      wTotorm   = wTotorm   + totd_orm
087500070215     C*
087600070215     C                   enddo
087700070215     C*
087800070215     C/EXEC SQL
087900070215     C+ close C10
088000070215     C/END-EXEC
088100070215     C*
088200071005     C                   evalr     stototorm = %editc(wTotorm  :'2')
088300070215     C*
088400070215     C* Chiusura printer-file
088500071005     C                   write     or80ri
088600071005     C                   write     or80tt
088700071005     C                   write     or80ri
088800071005     C                   write     or80er
088900071005     C                   write     or80bk
089000071005     C                   write     or80bk
089100071005     C                   write     or80fi
089200071005     C                   close     FIOR80P
089300070215     C*
089400070215     C                   ENDSR
089500070202     C*------------------------------------------------------------------------*
089600070202     C* VALDET - Routine valorizzazione dettaglio in stampa
089700070202     C*------------------------------------------------------------------------*
089800070202     C     VALDET        BEGSR
089900070202     C*
090000070202     C* Se sintetica....
090100070202     C                   if        *in40 = *off
090200071005     C   51              evalr     stptotorm =
090300071005     C                             %subst(%editc(totg_orm:'1'):7:5)
090400071005     C   51              eval      stpperc   = totg_orm * 100 / totd_orm
090500071005     C*  51              evalr     stptotncl =
090600071005     C*                            %subst(%editc(totg_ncl:'1'):7:5)
090700070202     C   51              evalr     stptotpkf =
090800070202     C                             %subst(%editc(totg_pes:'1'):6:6)
090900070202     C   51              evalr     stptotvlf =
091000070202     C                             %subst(%editc(totg_vol:'1'):8:5)
091100070202     C*
091200071005     C  N51              evalr     stptotorm =
091300071005     C                             %subst(%editc(totd_orm:'1'):7:5)
091400070202     C  N51              eval      stpperc   = *zeros
091500071005     C* N51              evalr     stptotncl =
091600071005     C*                            %subst(%editc(totd_ncl:'1'):7:5)
091700070202     C  N51              evalr     stptotpkf =
091800070202     C                             %subst(%editc(totd_pes:'1'):6:6)
091900070202     C  N51              evalr     stptotvlf =
092000070202     C                             %subst(%editc(totd_vol:'1'):8:5)
092100070202     C                   else
092200071005     C                   evalr     stptotorm =
092300071005     C                             %subst(%editc(totg_orm:'1'):7:5) + '/' +
092400071005     C                             %subst(%editc(totd_orm:'1'):7:5)
092500071005     C                   eval      stpperc   = totg_orm * 100 / totd_orm
092600071005     C*                  evalr     stptotncl =
092700071005     C*                            %subst(%editc(totg_ncl:'1'):7:5) + '/' +
092800071005     C*                            %subst(%editc(totd_ncl:'1'):7:5)
092900070202     C                   evalr     stptotpkf =
093000070202     C                             %subst(%editc(totg_pes:'1'):6:6) + '/' +
093100070202     C                             %subst(%editc(totd_pes:'1'):6:6)
093200070202     C                   evalr     stptotvlf =
093300070202     C                             %subst(%editc(totg_vol:'1'):8:5) + '/' +
093400070202     C                             %subst(%editc(totd_vol:'1'):8:5)
093500070202     C                   endif
093600070202     C*
093700070202     C                   ENDSR
093800061215     C*--------------------------------------------------------------------------------------------*
093900061215     C* REPERISCE I DATI UTENTE
094000061215     C*--------------------------------------------------------------------------------------------*
094100061215     C     REPDATIUTE    BEGSR
094200061215     C*
094300061215     C* INIZIALIZZA VARIABILI DI WRK
094400061215     C                   CLEAR                   TIBS34DS
094500061215     C                   CLEAR                   AZUTEDS
094600061215     C                   CLEAR                   DDATIUTE
094700061215     C*
094800061215     C     *DTAARA       DEFINE    §azute        azuteds
094900061215     C     *DTAARA       DEFINE    §datiute      ddatiute
095000061215     C                   IN(E)     *DTAARA
095100061215     C                   IF        %Error
095200061215     c                   EVAL      I34Tla = 'L'
095300061215     C                   CALL      'TIBS34R'
095400061215     C                   PARM                    Tibs34Ds
095500061215     C                   IN        *DTAARA
095600061215     C                   ENDIF
095700061215     C*
095800061215     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
095900061215     C                   MOVEL(P)  RSUT          DSFIRS           20
096000061215     C*
096100061215     C                   ENDSR
096200070202     C*--------------------------------------------------------------------------------------------*
096300070202     C* CARICA I P.O. IN "lira" richiesta
096400070202     C*--------------------------------------------------------------------------------------------*
096500070202     C     CARLIRA       BEGSR
096600070202     C*
096700070202     C* CARICO TABELLA FILIALI GESTITE £6
096800070202     C                   clear                   trul06ds
096900070202     C                   move      '£6'          d06cod
097000070205     C                   movel     In_FGS        d06key
097100070202     C                   movel     trul06ds      kpjbu
097200070202     C                   call      'TRUL06R'
097300070202     C                   parm                    kpjba
097400070202     C                   movel     kpjbu         trul06ds
097500070202     C*
097600070202     C                   ENDSR
097700060515     C*--------------------------------------------------------------------------------------------*
097800060515     C* *inzsr - operazioni iniziali
097900060515     C*--------------------------------------------------------------------------------------------*
098000000000     C     *inzsr        BEGSR
098100060515     C*
098200060515     C* Ricevimento parametri
098300060515     C     *ENTRY        PLIST
098400061214     C                   PARM                    KPJBA
098500061214     C*
098600061214     C* Ridefinisco i parametri
098700061214     C                   MOVEL     KPJBU         PARAM
098800061220     C*
098900061220     C* Verifico subito se richiesta stampa in dettaglio o sintetica
099000070215     C                   setoff                                       40
099100070215     C                   setoff                                       54
099200070215     C                   if        In_LIV  = 'D'
099300070215     C                   seton                                        40
099400070215     C                   endif
099500070215     C                   if        In_LIV  = 'T'
099600070215     C                   seton                                        54
099700070215     C                   endif
099800070226     C                   if        In_PIG  = 'A'
099900070226     C                   eval      In_PIG_num = 1
100000070226     C                   else
100100070226     C                   eval      In_PIG_num = 2
100200070226     C                   endif
100300070125     C*
100400070125     C* CALCOLA LA DATA CORRENTE
100500070125     C                   time                    wn14             14 0
100600070125     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
100700070125     C                   z-add     wn8           g08dat
100800070125     C                   z-add     *zeros        g08inv
100900070125     C                   movel     '0'           g08err
101000070125     C                   call      'XSRDA8'
101100070125     C                   parm                    wlbda8
101200070125     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
101900060515     C*
102000060515     C                   ENDSR
