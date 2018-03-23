000100010925      *PARMS DYNUSRPRF(*OWNER)
000200991026      /TITLE Download via Internet: decisore traduzione ASCII.
000300990908     H dftactgrp(*yes)
000400991026
000500991026      **********************************************************************
000600991026      * Legenda PRMESITO
000700991026      * ' ' = Elaborazione non eseguita.
000800991026      * '0' = Elaborazione eseguita senza errori.
000900991104      * 'A' = Parametro PRMKSU vuoto.
001000991026      * 'B' = Parametro PRMTIP vuoto o sconosciuto.
001100991026      * 'C' = Tipo file non previsto per il cliente.
001200991104      * 'D' = Cliente non abilitato al servizio.
001300031008      * 'E' = File/membro già allocato dalla procedura FTP/e-mail sul server
001400031008      * 'F' = Progressivo NON univoco  => ATTENZIONE, possibili errori gravi ed imprevedibili!!!
001500991026      **********************************************************************
001600991026
001700050909 TESTF*tivltnew  o    e             disk    RENAME(tivlt000:tivltn00) USROPN
001800991026     Ftivlt00f  o    e             disk
001900000712     Ftivlt07l  uf   e           k disk    RENAME(tivlt000:tivlt700)
002000031008     Ftivlt08l  if   e           k disk    RENAME(tivlt000:tivlt800)
002100031008     F                                     PREFIX(w_)
002200991105     Ftivss02l  if   e           k disk
002300990920     Ftivtf01l  if   e           k disk
002400991026     Ftivir02l  if   e           k disk
002500000707     Ftivre01l  if   e           k disk
002600000622     Ftntbe01l  if   e           k disk
002700000627     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002800000627     F                                     PREFIX(f_)
002900000627
003000020212     D disv          e ds
003100030526     D dvir01        e ds
003200991027     D psds           sds
003300991027     D  psjobname            244    253
003400991027     D  psuser               254    263
003500991027     D  psjobnbr             264    269s 0
003600991026     D prmksc          s                   like(virksc)
003700991026     D prmksu          s                   like(virksc)
003800991026     D prmtip          s                   like(virtip)
003900991026     D prmesito        s              1
004000000623     D prmprog         s                   like(vltprg)
004100000623     D prmsrv          s                   like(vltisv)
004200000628     D prmope          s              1
004300000713     D prmnfp          s                   like(vltfl2)
004400991026     D wrkksc          s                   like(prmksc)
004500991026     D wrkksu          s                   like(prmksu)
004600991026     D wrktip          s                   like(prmtip)
004700991026     D wrkesito        s                   like(prmesito)
004800991026     D wrkoggiiso      s               d
004900991026     D wrkoggi         s              8  0
005000991026     D wrkora          s              6  0
005100991026     D wrkmese         s              2  0
005200000623     D wrkprg          s              8  0 INZ(*zeros)
005300000711     D wrkvarie        s              1    INZ('0')
005400000712     D wrkmsg          s             80    INZ(*blanks)
005500000914     D vssok           s               n   INZ(*off)
005600051011
005700051011     D dwlprg          ds
005800051011     D  dwl_isv                       2    INZ(*all'0')
005900051011     D  dwl_tip                       2    INZ(*all'0')
006000051011     D  dwl_prg                       6    INZ(*all'0')
006100000711
006200991026     D                 ds                  inz
006300991026     D wrkday                  1      2s 0
006400991026     D wrkdaychr                      2    overlay(wrkday)
006500991026     D wrkpgt          s                   like(vtfpgt)
006600991026     D wrkppt          s                   like(vtfppt)
006700000711     D wrkpgi          s                   like(vtfpgi)
006800000711     D wrkppi          s                   like(vtfppi)
006900991026     D wrkfid          s                   like(vtffid)
007000991026     D wrkmbd          s                   like(vltmbd)
007100991026     D wrkfis          s                   like(vtffis)
007200991026     D wrkmbs          s                   like(vltmbs)
007300991026     D wrkfit          s                   like(vtffit)
007400991026     D wrkmbt          s                   like(vltmbd)
007500991026     D prmfid          s                   like(wrkfid)
007600991026     D prmmbd          s                   like(wrkmbd)
007700991026     D prmfis          s                   like(wrkfis)
007800991026     D prmmbs          s                   like(wrkmbs)
007900991026     D prmfit          s                   like(wrkfit)
008000991027     D prmmbt          s                   like(wrkmbt)
008100991026     D prmpgt          s                   like(wrkpgt)
008200991026     D prmppt          s                   like(wrkppt)
008300991026     D mc              s              1    dim(12) ctdata perrcd(12)
008400991104
008500991111     D mbrd01          s            266
008600991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
008700991111     D mbrd03          s              8    inz('MBRD0200')
008800991111     D mbrd04          ds
008900991111     D mbrd04a                       10                                         File
009000991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009100991111     D mbrd05          s             10                                         Membro
009200991125     D mbrd06          s              1    inz('0')
009300991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
009400991104     D rneta01         ds
009500991104     D  rneta01a                      8b 0
009600991104     D  rneta01b                      8b 0
009700991104     D  rneta01c                     24
009800991104     D rneta02         s              8b 0 inz(%size(rneta01))
009900991104     D rneta03         s              8b 0 inz(1)
010000991104     D rneta04         s             10    inz('SYSNAME')
010100991104     D nait            ds
010200991104     D  nait01                       10
010300991104     D  nait02                        1
010400991104     D  nait03                        1
010500991104     D  nait04                        8b 0
010600991104     D  sysname                       8
010700991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
010800991104
010900000622
011000000905     D*-------------------
011100000905     D* DS di ridefinizione file di log x passaggio recordset SQL
011200000905     D*-------------------
011300000905     D  dsvlt        E DS                  extname(tivlt00f)
011400000905     D                                     prefix(s_)
011500000905
011600040817
011700040817     D*-------------------
011800050909 TESTD* Variabili d wrk
011900040817     D*-------------------
012000050909     D*  COMMAND        S            300    inz(*blanks)
012100040817
012200000905
012300000905
012400000622      /TITLE *** MAIN ***
012500000622     C*
012600000906     C                   if        prmnfp  > '01' and
012700000906     C                             prmprog > *blanks
012800000905     C                   exsr      rtvdatora
012900000905     C                   else
013000000905     C                   time                    wrkoggiiso
013100000905     C                   time                    wrkora
013200000905     C     *iso          move      wrkoggiiso    wrkoggi
013300000905     C                   endif
013400030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
013500000328     C* Inizializzo variabili "wrk".
013600000328     C                   exsr      inzwrk
013700991026     C* Controllo parametri ricevuti.
013800991026     C                   exsr      chkprm
013900000626     C* Reperisco informazioni file.
014000000706     C                   exsr      rtvleg
014100000706     C* Alla fine...
014200000706     C                   exsr      uscita
014300000626
014400000626
014500000905
014600000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
014700000905     C*
014800000905     C     rtvdatora     begsr
014900000905     C*
015000000905     C/EXEC SQL
015100000905     C+ declare C1 cursor for select *
015200000905     C+ from tivlt00f where
015300000905     C+ vltprg = :prmprog
015400000905     C+ for fetch only
015500000905     C/END-EXEC
015600000905     C
015700000905     C/EXEC SQL
015800000905     C+ open C1
015900000905     C/END-EXEC
016000000905     C
016100000905     C/EXEC SQL
016200000905     C+ Fetch C1 into :dsvlt
016300000905     C/END-EXEC
016400000905     C*
016500000906     C                   if        sqlcod = *zeros     and
016600000906     C                             s_vltprg  = prmprog and
016700000906     C                             s_vltdat  > *zeros  and
016800000905     C                             s_vltora  > *zeros
016900000905     C                   z-add     s_vltdat      wrkoggi
017000000905     C                   z-add     s_vltora      wrkora
017100000905     C                   else
017200000905     C                   time                    wrkoggiiso
017300000905     C                   time                    wrkora
017400000905     C     *iso          move      wrkoggiiso    wrkoggi
017500000905     C                   endif
017600000905     C
017700000905     C/EXEC SQL
017800000905     C+ close C1
017900000905     C/END-EXEC
018000000905     C
018100000905     C*
018200000905     C                   endsr
018300000905
018400000905
018500000706
018600000706      /TITLE Determino eventuali legami cliente/tipi file
018700000706     C     rtvleg        begsr
018800000706     C*
018900000706     C                   eval      virksc = wrkksu
019000000706     C                   eval      virtip = wrktip
019100000706     C                   eval      virdti = wrkoggi
019200000707     C                   eval      vtftip = wrktip
019300000706     C* Reperisco informazioni file.
019400000706     C                   exsr      rtvtip
019500000706     C*
019600000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
019700000713     C                   if        wrkesito = '0'     and
019800000713     C                             prmprog  = *blanks and
019900000713     C                             prmope   = *blanks
020000000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
020100000711     C                   movel     '1'           wrkvarie
020200000706     C*
020300000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
020400000707     C     k03vir02      setll     tivre01l
020500000707     C                   if        %found(tivre01l)
020600000707     C     k03vir02      reade     tivre01l
020700000706     C                   dow       not %eof
020800000706     C                   eval      virksc = wrkksu
020900000706     C                   eval      virtip = vretfl
021000000706     C                   eval      virdti = wrkoggi
021100000707     C                   eval      vtftip = vretfl
021200000712     C                   eval      wrktip = vretfl
021300000706     C* Reperisco informazioni file.
021400000706     C                   exsr      rtvtip
021500000712     C                   eval      virtip = vretip
021600000707     C     k03vir02      reade     tivre01l
021700000706     C                   enddo
021800000706     C                   endif
021900000706     C*
022000000706     C                   endif
022100031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
022200031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
022300031008     C                   if        chkPRG <> '1'
022400030512     C                   if        chkFTP <> '1'
022500000711     C                   exsr      exepgm
022600030512     C                   endif
022700031008     C                   endif
022800000706     C*
022900000706     C                   endsr
023000000706
023100000706
023200000706
023300000706      /TITLE Reperisco tipo file.
023400000706     C     rtvtip        begsr
023500000706     C* Reperisco il tipo file.
023600000706     C     k01vtf01      chain     tivtf01l
023700000706     C* Se il tipo file non esiste
023800000706     C* oppure non è valido per il download.
023900000706     C                   if        not %found
024000000706     C                             or vtfud <> 'D'
024100031008     C*
024200031008     C                   exsr      inztivlt
024300031008     C                   eval      vltsts = '2'
024400031008     C                   eval      vltmsg =
024500031008     C                               %trim(sysname)
024600031008     C                             + '/'
024700031008     C                             + %trim(psjobname)
024800031008     C                             + '/'
024900031008     C                             + %trim(psuser)
025000031008     C                             + '/'
025100031008     C                             + %trim(%editc(psjobnbr:'Z'))
025200031008     C                             + ' Tipo file inesistente.'
025300031008     C                   exsr      wrttivlt
025400031008     C                   eval      wrkesito = 'B'
025500031008     C*
025600000721     C                   else
025700000706     C* Reperisco il tipo file del cliente.
025800000706     C     k03vir02      setgt     tivir02l
025900000706     C     k02vir02      readpe    tivir02l
026000000706     C* Tipo file non previsto per il cliente
026100000706     C* oppure scaduto.
026200000731     C                   if        %eof
026300000706     C                             or virdtf < wrkoggi
026400000706     C                             or virdti > wrkoggi
026500000706     C*
026600000731     C                   exsr      inztivltp
026700000706     C*
026800000706     C                   eval      vltsts = '2'
026900000706     C                   eval      vltmsg =
027000000706     C                               %trim(sysname)
027100000706     C                             + '/'
027200000706     C                             + %trim(psjobname)
027300000706     C                             + '/'
027400000706     C                             + %trim(psuser)
027500000706     C                             + '/'
027600000706     C                             + %trim(%editc(psjobnbr:'Z'))
027700000706     C                             + ' Tipo file non previsto per il cliente.'
027800000706     C                   exsr      wrttivlt
027900000706     C                   eval      wrkesito = 'C'
028000000706     C*
028100000706     C                   else
028200000706     C*
028300000706     C* Imposto i default.
028400000706     C                   if        virpgt <> *blanks
028500000706     C                   eval      wrkpgt = virpgt
028600000706     C                   eval      wrkppt = virppt
028700000706     C                   else
028800000706     C                   eval      wrkpgt = vtfpgt
028900000706     C                   eval      wrkppt = vtfppt
029000000706     C                   endif
029100000712     C*
029200000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
029300000711     C                   if        wrkvarie = '0'                               <===================
029400000711     C                   if        virpgi <> *blanks                            <===================
029500000711     C                   eval      wrkpgi = virpgi                              <===================
029600000711     C                   eval      wrkppi = virppi                              <===================
029700000711     C                   else                                                   <===================
029800000711     C                   eval      wrkpgi = vtfpgi                              <===================
029900000711     C                   eval      wrkppi = vtfppi                              <===================
030000000711     C                   endif                                                  <===================
030100000711     C                   endif                                                  <===================
030200000706     C* File da tradurre.
030300000706     C                   if        virfid <> *blanks
030400000706     C                   eval      wrkfid = virfid
030500000706     C                   else
030600000706     C                   eval      wrkfid = vtffid
030700000706     C                   endif
030800000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
030900000706     C* File storico.
031000000706     C                   if        virfis <> *blanks
031100000706     C                   eval      wrkfis = virfis
031200000706     C                   else
031300000706     C                   eval      wrkfis = vtffis
031400000706     C                   endif
031500000706     C                   extrct    wrkoggiiso:*m wrkmese
031600000706     C                   extrct    wrkoggiiso:*d wrkday
031700000706     C                   eval      wrkmbs =
031800000706     C                             mc(wrkmese)
031900000706     C                             + wrkdaychr
032000000706     C                             + %subst(wrkksu:2:7)
032100030519     C*
032200030519     C                   eval      vssisv = virfi1
032300030519     C* Valorizzo subito il progressivo applicazione.
032400030519     C                   exsr      calprog
032500000706     C* File tradotto.
032600000706     C                   if        virfit <> *blanks
032700000706     C                   eval      wrkfit = virfit
032800000706     C                   else
032900000706     C                   eval      wrkfit = vtffit
033000000706     C                   endif
033100030526     C* Tipo nome membro tradotto
033200030526     C                   eval      dvir01 = virpth
033300030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
033400030526     C                   if        §VIR01TM = 'P'
033500030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
033600030526 xxx C***                eval      wrkmbs = dwlprg
033700030519     C                   else
033800030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
033900030519     C                   endif
034000000706     C* Controllo abilitazione al servizio.
034100000706     C                   exsr      chkvss
034200000706     C                   endif
034300000706     C                   endif
034400000706     C*
034500000706     C                   endsr
034600000706
034700000711
034800000711
034900000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
035000000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
035100000711     C*
035200000711     C     exepgm        begsr
035300000711     C* Lancio il pgm in variabile con relativi parametri
035400000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
035500000711    C                   if        wrkpgi <> *blanks
035600000712     C* Chiamo il pgm post traduzione.
035700000712     C                   call(e)   'TIS7T9C1'
035800000712     C                   parm      wrkfid        prmfid
035900000712     C                   parm      wrkmbd        prmmbd
036000000712     C                   parm      wrkfis        prmfis
036100000712     C                   parm      wrkmbs        prmmbs
036200031106     C                   parm      wrkfit        prmfit
036300031106     C                   parm      wrkmbt        prmmbt
036400000712     C                   parm                    wrkpgi
036500000712     C                   parm                    wrkppi
036600000712     C                   parm      *blanks       prmesito
036700000712     C* Se la call è finita in errore
036800000712     C* o il lancio del PGM ha avuto degli errori.
036900000712     C                   if        %error
037000000712     C                             or prmesito <> '0'
037100000712     C     k01vlt        setll     tivlt07l
037200000712     C                   if        %found(tivlt07l)
037300000712     C     k01vlt        reade     tivlt07l
037400000712     C                   dow       not %eof
037500000712     C                   if        vltsqz = *zeros and
037600000712     C                             (vltsts = '0' or vltsts = '1')
037700000712     C                   eval      vltsts = '2'
037800000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
037900000712     C                   eval      vltmsg =
038000000712     C                               %trim(sysname)
038100000712     C                             + '/'
038200000712     C                             + %trim(psjobname)
038300000712     C                             + '/'
038400000712     C                             + %trim(psuser)
038500000712     C                             + '/'
038600000712     C                             + %trim(%editc(psjobnbr:'Z'))
038700000712     C                             + %trim(wrkmsg)
038800000712     C                   update    tivlt700
038900000712     C                   endif
039000000712     C     k01vlt        reade     tivlt07l
039100000712     C                   enddo
039200000712     C                   endif
039300000712     C                   endif
039400000712     C*
039500000711     C                   endif
039600000711     C*
039700000711     C                   endsr
039800000711
039900000711
040000000626
040100000626      /TITLE Valorizzazione Progressivo Applicazione
040200000626     C     calprog       begsr
040300000626     C*
040400000706     C                   if        dwlprg = *all'0'
040500000627     C                   read(e)   tis7prgf
040600000627     C                   if        not %error
040700000627     C                   eval      dwlprg = f_tis7prgf
040800000627     C*
040900000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
041000000719     C                   if        prmsrv <> *blanks
041100000719     C                   eval      vssisv = prmsrv
041200000719     C                   endif
041300000626     C*
041400000719     C                   if        prmprog = *blanks
041500051011     C                   move(p)   dwl_prg       wrkprg
041600000626     C                   add       1             wrkprg
041700051011     C                   move(p)   wrkprg        dwl_prg
041800051011     C                   movel     vssisv        dwl_isv
041900051011     C                   if        wrktip <> *blanks
042000051011     C                   movel     wrktip        dwl_tip
042100051011     C                   else
042200051011     C                   movel     'XX'          dwl_tip
042300051011     C                   endif
042400040123     C                   eval      f_tis7prgf = dwlprg
042500040123     C                   update    tis7prg0
042600000626     C                   else
042700040123     C                   unlock(e) tis7prgf
042800000626     C                   eval      dwlprg = prmprog
042900000626     C                   endif
043000000627     C*
043100000627     C                   endif
043200000706     C                   endif
043300000626     C*
043400000626     C                   endsr
043500991104
043600000706
043700000706
043800000706
043900991104      /TITLE Controllo abilitazione servizio.
044000991104     C     chkvss        begsr
044100991104     C*
044200000914     C                   eval      vssok = *off
044300000914     C*
044400991105     C                   eval      vssksu = wrkksu
044500000914     C     k02vss02      setll     tivss02l
044600000914     C     k02vss02      reade     tivss02l
044700000914     C                   dow       not %eof
044800000914     C                   if        vssdde <= wrkoggi and
044900000914     C                             vssdsc >= wrkoggi and
045000000914     C                             vssvat <> 'A'     and
045100000914     C                             vssvat <> 'S'
045200000914     C                   eval      vssok = *on
045300000914     C                   leave
045400000914     C                   endif
045500000914     C     k02vss02      reade     tivss02l
045600000914     C                   enddo
045700000914     C*
045800000914     C                   if        vssok = *off
045900991104     C*
046000000622     C                   exsr      inztivlt
046100000329     C                   eval      vltsts = '2'
046200991104     C                   eval      vltmsg =
046300991104     C                               %trim(sysname)
046400991104     C                             + '/'
046500991104     C                             + %trim(psjobname)
046600991104     C                             + '/'
046700991104     C                             + %trim(psuser)
046800991104     C                             + '/'
046900991104     C                             + %trim(%editc(psjobnbr:'Z'))
047000991104     C                             + ' Cliente non abilitato al servizio.'
047100000622     C                   exsr      wrttivlt
047200000622     C                   eval      wrkesito = 'D'
047300031008     C*
047400000622     C                   else
047500031008     C*
047600031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
047700031008     C* del progressivo rispetto a file/membro e cliente
047800031008     C                   movel     '0'           chkPRG            1
047900031008     C     k08vlt        setll     tivlt08l
048000031008     C                   if        %equal(tivlt08l)
048100031008     C     k08vlt        reade     tivlt08l
048200031008     C                   dow       not %eof(tivlt08l)
048300031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
048400031008     C                   if        w_vltksc <> wrkksc
048500031008     C                   movel     '1'           chkPRG
048600031008     C                   endif
048700031008     C     k08vlt        reade     tivlt08l
048800031008     C                   enddo
048900031008     C                   endif
049000031008     C*
049100000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
049200031008     C                   if        chkPRG = '0'
049300000706     C                   exsr      traduci
049400031008     C                   else
049500031008     C*
049600031008     C                   exsr      inztivlt
049700031008     C                   eval      vltsts = '2'
049800031008     C                   eval      vltmsg =
049900031008     C                               %trim(sysname)
050000031008     C                             + '/'
050100031008     C                             + %trim(psjobname)
050200031008     C                             + '/'
050300031008     C                             + %trim(psuser)
050400031008     C                             + '/'
050500031008     C                             + %trim(%editc(psjobnbr:'Z'))
050600031008     C                             + ' Progressivo/membro NON univoco!!!'
050700031008     C                   exsr      wrttivlt
050800031008     C                   eval      wrkesito = 'F'
050900031008     C                   exsr      sndeml
051000031008     C                   exsr      uscita
051100031008     C                   endif
051200991104     C*
051300991104     C                   endif
051400991104     C*
051500991104     C                   endsr
051600000622
051700000622
051800000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
051900000622     C     rtvtia        begsr
052000000622     C*
052100000622     C                   eval      tbecod = 'ISV'
052200000731     C                   eval      tbeke1 = virfi1
052300000622     C     keytbe        chain     tntbe01l
052400000622     C                   if        %found(tntbe01l)
052500020212     C                   movel     tbeuni        disv
052600000622     C                   endif
052700000622     C*
052800000622     C                   endsr
052900000706
053000000706
053100000706
053200000706      /TITLE Traduzione.
053300000706     C     traduci       begsr
053400031105     C*
053500031105     C* Reperisco informazioni membro da tradurre.
053600031105     C                   eval      mbrd04a = wrkfid
053700031105     C                   eval      mbrd05  = wrkmbd
053800031105     C                   exsr      rtvmbrd
053900031105     C* Controllo se il membro da tradurre ha dei record.
054000031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
054100031105     C                   if        (qusei=*blanks and
054200031105     C                             qusnbrcr > 0) or prmnfp > '01'
054300030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
054400030512     C                   call      'TIS7T9R2'
054500031008     C                   parm                    wrkfit
054600031008     C                   parm                    wrkmbt
054700030512     C                   parm      '0'           chkFTP            1
054800031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
054900031105     C                   if        chkFTP = '0'
055000000706     C*
055100000706     C                   exsr      inztivlt
055200000706     C* Chiamo il pgm traduttore.
055300000706     C                   call(e)   'TIS7T9C'
055400000706     C                   parm      wrkfid        prmfid
055500000706     C                   parm      wrkmbd        prmmbd
055600000706     C                   parm      wrkfis        prmfis
055700000706     C                   parm      wrkmbs        prmmbs
055800000706     C                   parm      wrkfit        prmfit
055900000706     C                   parm      wrkmbt        prmmbt
056000000706     C                   parm      wrkpgt        prmpgt
056100000706     C                   parm      wrkppt        prmppt
056200000706     C                   parm      *blanks       prmesito
056300000706     C* Se la call è finita in errore
056400000706     C* o la traduzione ha avuto degli errori.
056500000706     C                   if        %error
056600000712     C                             or prmesito <> '0'
056700000706     C                   eval      vltsts = '2'
056800000706     C                   eval      vltmsg =
056900000706     C                               %trim(sysname)
057000000706     C                             + '/'
057100000706     C                             + %trim(psjobname)
057200000706     C                             + '/'
057300000706     C                             + %trim(psuser)
057400000706     C                             + '/'
057500000706     C                             + %trim(%editc(psjobnbr:'Z'))
057600000706     C                             + ' Errori nella traduzione.'
057700000706     C                   else
057800000706     C                   eval      wrkesito = '0'
057900000706     C                   endif
058000000706     C*
058100000706     C                   exsr      wrttivlt
058200031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
058300031105     C                   else
058400030512     C                   exsr      inztivlt
058500030512     C                   eval      vltsts = '2'
058600030512     C                   eval      vltmsg =
058700030512     C                               %trim(sysname)
058800030512     C                             + '/'
058900030512     C                             + %trim(psjobname)
059000030512     C                             + '/'
059100030512     C                             + %trim(psuser)
059200030512     C                             + '/'
059300030512     C                             + %trim(%editc(psjobnbr:'Z'))
059400030512     C                             + ' Cliente/TipoFile in uso da server FTP'
059500030512     C                   exsr      wrttivlt
059600030512     C                   eval      wrkesito = 'E'
059700030512     C                   endif
059800031105     C* NO record da tradurre
059900031105     C                   endif
060000000706     C*
060100000706     C                   endsr
060200000328
060300000706
060400000706
060500000328      /TITLE Inizializzo variabili "wrk".
060600000328     C     inzwrk        begsr
060700000328     C*
060800000328     C                   clear                   wrkfit
060900000328     C                   clear                   wrkmbt
061000000328     C                   clear                   wrkpgi
061100000328     C                   clear                   wrkppi
061200000328     C                   clear                   wrkfis
061300000328     C                   clear                   wrkmbs
061400000328     C*
061500000328     C                   endsr
061600991026
061700000706
061800000706
061900991026      /TITLE Inizializzo TIVLT00F.
062000991026     C     inztivlt      begsr
062100000804     C*
062200000804     C                   exsr      rtvtia
062300991026     C*
062400000328     C                   clear                   tivlt000
062500991026     C                   eval      vltksc = wrkksc
062600991026     C                   eval      vltksu = wrkksu
062700000721     C                   eval      vlttip = wrktip
062800991026     C                   eval      vltfld = wrkfit
062900991026     C                   eval      vltmbd = wrkmbt
063000991026     C                   eval      vltpgi = wrkpgi
063100991026     C                   eval      vltppi = wrkppi
063200020212     C                   if        §isvproc = 'F'
063300000804     C                   eval      vltsnd = 'D'
063400000804     C                   else
063500991026     C                   eval      vltsnd = vtfsnd
063600000804     C                   endif
063700991026     C                   eval      vltfls = wrkfis
063800991026     C                   eval      vltmbs = wrkmbs
063900991026     C                   eval      vltdat = wrkoggi
064000991115     C                   eval      vltora = wrkora
064100991026     C                   eval      vltsts = '0'
064200991027     C                   eval      vltmsg =
064300991104     C                               %trim(sysname)
064400991104     C                             + '/'
064500991104     C                             + %trim(psjobname)
064600991027     C                             + '/'
064700991027     C                             + %trim(psuser)
064800991027     C                             + '/'
064900991027     C                             + %trim(%editc(psjobnbr:'Z'))
065000000328     C                   z-add     *zeros        vltsqz
065100000731     C*
065200991026     C                   endsr
065300991026
065400000706
065500000731
065600000731      /TITLE Inizializzo parzialmente TIVLT00F.
065700000731     C     inztivltp     begsr
065800000731     C*
065900000731     C                   clear                   tivlt000
066000000731     C                   eval      vltksc = wrkksc
066100000731     C                   eval      vltksu = wrkksu
066200000731     C                   eval      vlttip = wrktip
066300000731     C                   eval      vltpgi = wrkpgi
066400000731     C                   eval      vltppi = wrkppi
066500000731     C                   eval      vltsnd = vtfsnd
066600000731     C                   eval      vltdat = wrkoggi
066700000731     C                   eval      vltora = wrkora
066800000731     C                   eval      vltsts = '0'
066900000731     C                   eval      vltmsg =
067000000731     C                               %trim(sysname)
067100000731     C                             + '/'
067200000731     C                             + %trim(psjobname)
067300000731     C                             + '/'
067400000731     C                             + %trim(psuser)
067500000731     C                             + '/'
067600000731     C                             + %trim(%editc(psjobnbr:'Z'))
067700000731     C                   z-add     *zeros        vltsqz
067800000731     C*
067900000731     C                   exsr      rtvtia
068000000731     C*
068100000731     C                   endsr
068200000731
068300000731
068400000706
068500991026      /TITLE Scrittura TIVLT00F.
068600000731     C     wrttivlt      begsr
068700991026     C*
068800000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
068900000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
069000000731     C                   eval      vltisv = virfi1
069100020212     C                   eval      vlttia = §isvproc
069200000622     C                   eval      vltprg = dwlprg
069300000713     C                   eval      vltfl2 = prmnfp
069400991026     C                   write     tivlt000
069500040817     C*
069600050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
069700040817     C* procedura scambio dati
069800040817     C* ...quindi eseguo prima d tutto l'override
069900050909  "  C*
070000050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
070100050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
070200050909     C*                  eval      LUNG = %len(%trim(COMMAND))
070300050909  "  C*                  call(e)   'QCMDEXC'
070400050909     C*                  parm                    COMMAND
070500050909     C*                  parm                    LUNG             15 5
070600040817     C*
070700050909  "  C*                  if        not %error
070800050909     C*                  if        not %open(tivltnew)
070900050909     C*                  open(e)   tivltnew
071000050909     C*                  endif
071100050909  "  C*                  if        not %error
071200050909     C*                  write     tivltn00
071300050909     C*                  endif
071400050909     C*                  if        %open(tivltnew)
071500050909  "  C*                  close(e)  tivltnew
071600050909     C*                  endif
071700050909 TESTC*                  endif
071800991026     C*
071900991026     C                   endsr
072000991026
072100000706
072200000706
072300991026      /TITLE Operazioni finali.
072400991026     C     uscita        begsr
072500991026     C*
072600000627     C                   seton                                        lr
072700991026     C*
072800991026     C                   endsr
072900991026
073000000706
073100000706
073200991026      /TITLE Controllo formale parametri ricevuti.
073300991026     C     chkprm        begsr
073400991026     C*
073500991104     C                   if        wrkksu = *blanks
073600031008     C*
073700031008     C                   exsr      inztivlt
073800031008     C                   eval      vltsts = '2'
073900031008     C                   eval      vltmsg =
074000031008     C                               %trim(sysname)
074100031008     C                             + '/'
074200031008     C                             + %trim(psjobname)
074300031008     C                             + '/'
074400031008     C                             + %trim(psuser)
074500031008     C                             + '/'
074600031008     C                             + %trim(%editc(psjobnbr:'Z'))
074700031008     C                             + ' Parametro codice cliente non passato.'
074800031008     C                   exsr      wrttivlt
074900031008     C                   eval      wrkesito = 'A'
075000031008     C*
075100991026     C                   exsr      uscita
075200991026     C                   endif
075300991026     C*
075400991026     C                   if        wrktip = *blanks
075500031008     C*
075600031008     C                   exsr      inztivlt
075700031008     C                   eval      vltsts = '2'
075800031008     C                   eval      vltmsg =
075900031008     C                               %trim(sysname)
076000031008     C                             + '/'
076100031008     C                             + %trim(psjobname)
076200031008     C                             + '/'
076300031008     C                             + %trim(psuser)
076400031008     C                             + '/'
076500031008     C                             + %trim(%editc(psjobnbr:'Z'))
076600031008     C                             + ' Parametro tipo file non passato.'
076700031008     C                   exsr      wrttivlt
076800031008     C                   eval      wrkesito = 'B'
076900031008     C*
077000991026     C                   exsr      uscita
077100991026     C                   endif
077200991026     C*
077300991026     C                   clear                   wrkesito
077400991026     C*
077500991026     C                   endsr
077600991111
077700000706
077800000706
077900991111      /TITLE Reperisco informazioni membro.
078000991111     C     rtvmbrd       begsr
078100991111     C* Pulisco e imposto la DS degli errori.
078200000124     C                   clear                   qusec
078300991111     C                   eval      qusbprv = %size(qusec)
078400991111     C*
078500991111     C                   call      'QUSRMBRD'
078600991111     C     qusm0200      parm                    mbrd01
078700991111     C                   parm                    mbrd02
078800991111     C                   parm                    mbrd03
078900991111     C                   parm                    mbrd04
079000991111     C                   parm                    mbrd05
079100991111     C                   parm                    mbrd06
079200991111     C                   parm                    qusec
079300991111     C*
079400991111     C                   endsr
079500031008
079600031008
079700031008
079800031008
079900031008      /TITLE Compongo il testo e spedisco una e-m@ail
080000031008     C     sndeml        begsr
080100031008     C*
080200031008     C* Inizializzo variabili
080300031008     C                   movel     *blanks       wrkEml          253
080400031008     C                   movel     *blanks       wrkEmlMsg      5000
080500031008     C                   movel     *blanks       wrkEmlOgg        44
080600031008     C* Valorizzo i campi della e-m@ail
080700120305     C                   eval      wrkEml='cedalert@brt.it'
080800031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
080900031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
081000031008     C                             'SU AS/400'                         +':/N'+
081100031008     C                             ' '                                 +':/N'+
081200031008     C                             'RIFERIMENTI'                       +':/N'+
081300031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
081400031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
081500031008     C                             'Cliente:'+vltksc                   +':/N'+
081600031008     C                             'TipoFile:'+vlttip                  +':/N'+
081700031008     C                             ' '                                 +':/N'+
081800031008     C                             'MESSAGGIO'                         +':/N'+
081900031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
082000031008     C*
082100031008     C                   call(e)   'TIS701C'
082200031008     C                   parm                    wrkEml
082300031008     C                   parm                    wrkEmlOgg
082400031008     C                   parm                    wrkEmlMsg
082500031008     C*
082600031008     C                   endsr
082700990910
082800000706
082900000706
083000990908      /TITLE Operazioni iniziali.
083100990908     C     *inzsr        begsr
083200990908     C*
083300990908     C     *entry        plist
083400991026     C     wrkksc        parm                    prmksc
083500991026     C     wrkksu        parm      wrkksu        prmksu
083600991026     C     wrktip        parm                    prmtip
083700991026     C                   parm      wrkesito      prmesito
083800000623     C                   parm                    prmprog
083900000623     C                   parm                    prmsrv
084000000628     C                   parm                    prmope
084100000713     C                   parm                    prmnfp
084200991104     C*
084300991105     C     k02vss02      klist
084400991105     C                   kfld                    vssksu
084500991104     C                   kfld                    vssisv
084600991026     C*
084700991026     C     k03vir02      klist
084800991026     C                   kfld                    virksc
084900991026     C                   kfld                    virtip
085000991026     C                   kfld                    virdti
085100991026     C*
085200991026     C     k02vir02      klist
085300991026     C                   kfld                    virksc
085400991026     C                   kfld                    virtip
085500000706     C*
085600000706     C     k04vir02      klist
085700000706     C                   kfld                    vreksc
085800000706     C                   kfld                    vretfl
085900000706     C                   kfld                    vredti
086000990920     C*
086100990920     C     k01vtf01      klist
086200990920     C                   kfld                    vtftip
086300000622     C*
086400000622     C     keytbe        klist
086500000622     C                   kfld                    tbecod
086600000622     C                   kfld                    tbeke1
086700000712     C*
086800000712     C     k01vlt        klist
086900000712     C                   kfld                    vltprg
087000000712     C                   kfld                    vltksc
087100031008     C*
087200031008     C     k08vlt        klist
087300031008     C                   kfld                    wrkfit
087400031008     C                   kfld                    wrkmbt
087500000706     C*
087600991104     C* Reperisco nome sistema.
087700991104     C                   eval      qusbprv = %size(qusec)
087800991104     C                   call      'QWCRNETA'
087900991104     C                   parm                    rneta01
088000991104     C                   parm                    rneta02
088100991104     C                   parm                    rneta03
088200991104     C                   parm                    rneta04
088300991104     C                   parm                    qusec
088400991104     C                   eval      nait = rneta01c
088500990908     C*
088600990908     C                   endsr
088700991026** ctdata mc
088800991026ABCDEFGHILMN
