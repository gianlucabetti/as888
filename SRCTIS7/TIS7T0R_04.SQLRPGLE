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
005500000622     D dwlprg          s             10    INZ(*all'0')
005600000914     D vssok           s               n   INZ(*off)
005700000711
005800991026     D                 ds                  inz
005900991026     D wrkday                  1      2s 0
006000991026     D wrkdaychr                      2    overlay(wrkday)
006100991026     D wrkpgt          s                   like(vtfpgt)
006200991026     D wrkppt          s                   like(vtfppt)
006300000711     D wrkpgi          s                   like(vtfpgi)
006400000711     D wrkppi          s                   like(vtfppi)
006500991026     D wrkfid          s                   like(vtffid)
006600991026     D wrkmbd          s                   like(vltmbd)
006700991026     D wrkfis          s                   like(vtffis)
006800991026     D wrkmbs          s                   like(vltmbs)
006900991026     D wrkfit          s                   like(vtffit)
007000991026     D wrkmbt          s                   like(vltmbd)
007100991026     D prmfid          s                   like(wrkfid)
007200991026     D prmmbd          s                   like(wrkmbd)
007300991026     D prmfis          s                   like(wrkfis)
007400991026     D prmmbs          s                   like(wrkmbs)
007500991026     D prmfit          s                   like(wrkfit)
007600991027     D prmmbt          s                   like(wrkmbt)
007700991026     D prmpgt          s                   like(wrkpgt)
007800991026     D prmppt          s                   like(wrkppt)
007900991026     D mc              s              1    dim(12) ctdata perrcd(12)
008000991104
008100991111     D mbrd01          s            266
008200991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
008300991111     D mbrd03          s              8    inz('MBRD0200')
008400991111     D mbrd04          ds
008500991111     D mbrd04a                       10                                         File
008600991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
008700991111     D mbrd05          s             10                                         Membro
008800991125     D mbrd06          s              1    inz('0')
008900991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
009000991104     D rneta01         ds
009100991104     D  rneta01a                      8b 0
009200991104     D  rneta01b                      8b 0
009300991104     D  rneta01c                     24
009400991104     D rneta02         s              8b 0 inz(%size(rneta01))
009500991104     D rneta03         s              8b 0 inz(1)
009600991104     D rneta04         s             10    inz('SYSNAME')
009700991104     D nait            ds
009800991104     D  nait01                       10
009900991104     D  nait02                        1
010000991104     D  nait03                        1
010100991104     D  nait04                        8b 0
010200991104     D  sysname                       8
010300991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
010400991104
010500000622
010600000905     D*-------------------
010700000905     D* DS di ridefinizione file di log x passaggio recordset SQL
010800000905     D*-------------------
010900000905     D  dsvlt        E DS                  extname(tivlt00f)
011000000905     D                                     prefix(s_)
011100000905
011200040817
011300040817     D*-------------------
011400050909 TESTD* Variabili d wrk
011500040817     D*-------------------
011600050909     D*  COMMAND        S            300    inz(*blanks)
011700040817
011800000905
011900000905
012000000622      /TITLE *** MAIN ***
012100000622     C*
012200000906     C                   if        prmnfp  > '01' and
012300000906     C                             prmprog > *blanks
012400000905     C                   exsr      rtvdatora
012500000905     C                   else
012600000905     C                   time                    wrkoggiiso
012700000905     C                   time                    wrkora
012800000905     C     *iso          move      wrkoggiiso    wrkoggi
012900000905     C                   endif
013000030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
013100000328     C* Inizializzo variabili "wrk".
013200000328     C                   exsr      inzwrk
013300991026     C* Controllo parametri ricevuti.
013400991026     C                   exsr      chkprm
013500000626     C* Reperisco informazioni file.
013600000706     C                   exsr      rtvleg
013700000706     C* Alla fine...
013800000706     C                   exsr      uscita
013900000626
014000000626
014100000905
014200000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
014300000905     C*
014400000905     C     rtvdatora     begsr
014500000905     C*
014600000905     C/EXEC SQL
014700000905     C+ declare C1 cursor for select *
014800000905     C+ from tivlt00f where
014900000905     C+ vltprg = :prmprog
015000000905     C+ for fetch only
015100000905     C/END-EXEC
015200000905     C
015300000905     C/EXEC SQL
015400000905     C+ open C1
015500000905     C/END-EXEC
015600000905     C
015700000905     C/EXEC SQL
015800000905     C+ Fetch C1 into :dsvlt
015900000905     C/END-EXEC
016000000905     C*
016100000906     C                   if        sqlcod = *zeros     and
016200000906     C                             s_vltprg  = prmprog and
016300000906     C                             s_vltdat  > *zeros  and
016400000905     C                             s_vltora  > *zeros
016500000905     C                   z-add     s_vltdat      wrkoggi
016600000905     C                   z-add     s_vltora      wrkora
016700000905     C                   else
016800000905     C                   time                    wrkoggiiso
016900000905     C                   time                    wrkora
017000000905     C     *iso          move      wrkoggiiso    wrkoggi
017100000905     C                   endif
017200000905     C
017300000905     C/EXEC SQL
017400000905     C+ close C1
017500000905     C/END-EXEC
017600000905     C
017700000905     C*
017800000905     C                   endsr
017900000905
018000000905
018100000706
018200000706      /TITLE Determino eventuali legami cliente/tipi file
018300000706     C     rtvleg        begsr
018400000706     C*
018500000706     C                   eval      virksc = wrkksu
018600000706     C                   eval      virtip = wrktip
018700000706     C                   eval      virdti = wrkoggi
018800000707     C                   eval      vtftip = wrktip
018900000706     C* Reperisco informazioni file.
019000000706     C                   exsr      rtvtip
019100000706     C*
019200000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
019300000713     C                   if        wrkesito = '0'     and
019400000713     C                             prmprog  = *blanks and
019500000713     C                             prmope   = *blanks
019600000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
019700000711     C                   movel     '1'           wrkvarie
019800000706     C*
019900000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
020000000707     C     k03vir02      setll     tivre01l
020100000707     C                   if        %found(tivre01l)
020200000707     C     k03vir02      reade     tivre01l
020300000706     C                   dow       not %eof
020400000706     C                   eval      virksc = wrkksu
020500000706     C                   eval      virtip = vretfl
020600000706     C                   eval      virdti = wrkoggi
020700000707     C                   eval      vtftip = vretfl
020800000712     C                   eval      wrktip = vretfl
020900000706     C* Reperisco informazioni file.
021000000706     C                   exsr      rtvtip
021100000712     C                   eval      virtip = vretip
021200000707     C     k03vir02      reade     tivre01l
021300000706     C                   enddo
021400000706     C                   endif
021500000706     C*
021600000706     C                   endif
021700031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
021800031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
021900031008     C                   if        chkPRG <> '1'
022000030512     C                   if        chkFTP <> '1'
022100000711     C                   exsr      exepgm
022200030512     C                   endif
022300031008     C                   endif
022400000706     C*
022500000706     C                   endsr
022600000706
022700000706
022800000706
022900000706      /TITLE Reperisco tipo file.
023000000706     C     rtvtip        begsr
023100000706     C* Reperisco il tipo file.
023200000706     C     k01vtf01      chain     tivtf01l
023300000706     C* Se il tipo file non esiste
023400000706     C* oppure non è valido per il download.
023500000706     C                   if        not %found
023600000706     C                             or vtfud <> 'D'
023700031008     C*
023800031008     C                   exsr      inztivlt
023900031008     C                   eval      vltsts = '2'
024000031008     C                   eval      vltmsg =
024100031008     C                               %trim(sysname)
024200031008     C                             + '/'
024300031008     C                             + %trim(psjobname)
024400031008     C                             + '/'
024500031008     C                             + %trim(psuser)
024600031008     C                             + '/'
024700031008     C                             + %trim(%editc(psjobnbr:'Z'))
024800031008     C                             + ' Tipo file inesistente.'
024900031008     C                   exsr      wrttivlt
025000031008     C                   eval      wrkesito = 'B'
025100031008     C*
025200000721     C                   else
025300000706     C* Reperisco il tipo file del cliente.
025400000706     C     k03vir02      setgt     tivir02l
025500000706     C     k02vir02      readpe    tivir02l
025600000706     C* Tipo file non previsto per il cliente
025700000706     C* oppure scaduto.
025800000731     C                   if        %eof
025900000706     C                             or virdtf < wrkoggi
026000000706     C                             or virdti > wrkoggi
026100000706     C*
026200000731     C                   exsr      inztivltp
026300000706     C*
026400000706     C                   eval      vltsts = '2'
026500000706     C                   eval      vltmsg =
026600000706     C                               %trim(sysname)
026700000706     C                             + '/'
026800000706     C                             + %trim(psjobname)
026900000706     C                             + '/'
027000000706     C                             + %trim(psuser)
027100000706     C                             + '/'
027200000706     C                             + %trim(%editc(psjobnbr:'Z'))
027300000706     C                             + ' Tipo file non previsto per il cliente.'
027400000706     C                   exsr      wrttivlt
027500000706     C                   eval      wrkesito = 'C'
027600000706     C*
027700000706     C                   else
027800000706     C*
027900000706     C* Imposto i default.
028000000706     C                   if        virpgt <> *blanks
028100000706     C                   eval      wrkpgt = virpgt
028200000706     C                   eval      wrkppt = virppt
028300000706     C                   else
028400000706     C                   eval      wrkpgt = vtfpgt
028500000706     C                   eval      wrkppt = vtfppt
028600000706     C                   endif
028700000712     C*
028800000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
028900000711     C                   if        wrkvarie = '0'                               <===================
029000000711     C                   if        virpgi <> *blanks                            <===================
029100000711     C                   eval      wrkpgi = virpgi                              <===================
029200000711     C                   eval      wrkppi = virppi                              <===================
029300000711     C                   else                                                   <===================
029400000711     C                   eval      wrkpgi = vtfpgi                              <===================
029500000711     C                   eval      wrkppi = vtfppi                              <===================
029600000711     C                   endif                                                  <===================
029700000711     C                   endif                                                  <===================
029800000706     C* File da tradurre.
029900000706     C                   if        virfid <> *blanks
030000000706     C                   eval      wrkfid = virfid
030100000706     C                   else
030200000706     C                   eval      wrkfid = vtffid
030300000706     C                   endif
030400000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
030500000706     C* File storico.
030600000706     C                   if        virfis <> *blanks
030700000706     C                   eval      wrkfis = virfis
030800000706     C                   else
030900000706     C                   eval      wrkfis = vtffis
031000000706     C                   endif
031100000706     C                   extrct    wrkoggiiso:*m wrkmese
031200000706     C                   extrct    wrkoggiiso:*d wrkday
031300000706     C                   eval      wrkmbs =
031400000706     C                             mc(wrkmese)
031500000706     C                             + wrkdaychr
031600000706     C                             + %subst(wrkksu:2:7)
031700030519     C*
031800030519     C                   eval      vssisv = virfi1
031900030519     C* Valorizzo subito il progressivo applicazione.
032000030519     C                   exsr      calprog
032100000706     C* File tradotto.
032200000706     C                   if        virfit <> *blanks
032300000706     C                   eval      wrkfit = virfit
032400000706     C                   else
032500000706     C                   eval      wrkfit = vtffit
032600000706     C                   endif
032700030526     C* Tipo nome membro tradotto
032800030526     C                   eval      dvir01 = virpth
032900030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
033000030526     C                   if        §VIR01TM = 'P'
033100030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
033200030526 xxx C***                eval      wrkmbs = dwlprg
033300030519     C                   else
033400030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
033500030519     C                   endif
033600000706     C* Controllo abilitazione al servizio.
033700000706     C                   exsr      chkvss
033800000706     C                   endif
033900000706     C                   endif
034000000706     C*
034100000706     C                   endsr
034200000706
034300000711
034400000711
034500000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
034600000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
034700000711     C*
034800000711     C     exepgm        begsr
034900000711     C* Lancio il pgm in variabile con relativi parametri
035000000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
035100000711    C                   if        wrkpgi <> *blanks
035200000712     C* Chiamo il pgm post traduzione.
035300000712     C                   call(e)   'TIS7T9C1'
035400000712     C                   parm      wrkfid        prmfid
035500000712     C                   parm      wrkmbd        prmmbd
035600000712     C                   parm      wrkfis        prmfis
035700000712     C                   parm      wrkmbs        prmmbs
035800031106     C                   parm      wrkfit        prmfit
035900031106     C                   parm      wrkmbt        prmmbt
036000000712     C                   parm                    wrkpgi
036100000712     C                   parm                    wrkppi
036200000712     C                   parm      *blanks       prmesito
036300000712     C* Se la call è finita in errore
036400000712     C* o il lancio del PGM ha avuto degli errori.
036500000712     C                   if        %error
036600000712     C                             or prmesito <> '0'
036700000712     C     k01vlt        setll     tivlt07l
036800000712     C                   if        %found(tivlt07l)
036900000712     C     k01vlt        reade     tivlt07l
037000000712     C                   dow       not %eof
037100000712     C                   if        vltsqz = *zeros and
037200000712     C                             (vltsts = '0' or vltsts = '1')
037300000712     C                   eval      vltsts = '2'
037400000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
037500000712     C                   eval      vltmsg =
037600000712     C                               %trim(sysname)
037700000712     C                             + '/'
037800000712     C                             + %trim(psjobname)
037900000712     C                             + '/'
038000000712     C                             + %trim(psuser)
038100000712     C                             + '/'
038200000712     C                             + %trim(%editc(psjobnbr:'Z'))
038300000712     C                             + %trim(wrkmsg)
038400000712     C                   update    tivlt700
038500000712     C                   endif
038600000712     C     k01vlt        reade     tivlt07l
038700000712     C                   enddo
038800000712     C                   endif
038900000712     C                   endif
039000000712     C*
039100000711     C                   endif
039200000711     C*
039300000711     C                   endsr
039400000711
039500000711
039600000626
039700000626      /TITLE Valorizzazione Progressivo Applicazione
039800000626     C     calprog       begsr
039900000626     C*
040000000706     C                   if        dwlprg = *all'0'
040100000627     C                   read(e)   tis7prgf
040200000627     C                   if        not %error
040300000627     C                   eval      dwlprg = f_tis7prgf
040400000627     C*
040500000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
040600000719     C                   if        prmsrv <> *blanks
040700000719     C                   eval      vssisv = prmsrv
040800000719     C                   endif
040900000626     C*
041000000719     C                   if        prmprog = *blanks
041100000626     C                   move(p)   dwlprg        wrkprg
041200000626     C                   add       1             wrkprg
041300000626     C                   move(p)   wrkprg        dwlprg
041400000731     C                   movel     vssisv        dwlprg
041500040123     C                   eval      f_tis7prgf = dwlprg
041600040123     C                   update    tis7prg0
041700000626     C                   else
041800040123     C                   unlock(e) tis7prgf
041900000626     C                   eval      dwlprg = prmprog
042000000626     C                   endif
042100000627     C*
042200000627     C                   endif
042300000706     C                   endif
042400000626     C*
042500000626     C                   endsr
042600991104
042700000706
042800000706
042900000706
043000991104      /TITLE Controllo abilitazione servizio.
043100991104     C     chkvss        begsr
043200991104     C*
043300000914     C                   eval      vssok = *off
043400000914     C*
043500991105     C                   eval      vssksu = wrkksu
043600000914     C     k02vss02      setll     tivss02l
043700000914     C     k02vss02      reade     tivss02l
043800000914     C                   dow       not %eof
043900000914     C                   if        vssdde <= wrkoggi and
044000000914     C                             vssdsc >= wrkoggi and
044100000914     C                             vssvat <> 'A'     and
044200000914     C                             vssvat <> 'S'
044300000914     C                   eval      vssok = *on
044400000914     C                   leave
044500000914     C                   endif
044600000914     C     k02vss02      reade     tivss02l
044700000914     C                   enddo
044800000914     C*
044900000914     C                   if        vssok = *off
045000991104     C*
045100000622     C                   exsr      inztivlt
045200000329     C                   eval      vltsts = '2'
045300991104     C                   eval      vltmsg =
045400991104     C                               %trim(sysname)
045500991104     C                             + '/'
045600991104     C                             + %trim(psjobname)
045700991104     C                             + '/'
045800991104     C                             + %trim(psuser)
045900991104     C                             + '/'
046000991104     C                             + %trim(%editc(psjobnbr:'Z'))
046100991104     C                             + ' Cliente non abilitato al servizio.'
046200000622     C                   exsr      wrttivlt
046300000622     C                   eval      wrkesito = 'D'
046400031008     C*
046500000622     C                   else
046600031008     C*
046700031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
046800031008     C* del progressivo rispetto a file/membro e cliente
046900031008     C                   movel     '0'           chkPRG            1
047000031008     C     k08vlt        setll     tivlt08l
047100031008     C                   if        %equal(tivlt08l)
047200031008     C     k08vlt        reade     tivlt08l
047300031008     C                   dow       not %eof(tivlt08l)
047400031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
047500031008     C                   if        w_vltksc <> wrkksc
047600031008     C                   movel     '1'           chkPRG
047700031008     C                   endif
047800031008     C     k08vlt        reade     tivlt08l
047900031008     C                   enddo
048000031008     C                   endif
048100031008     C*
048200000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
048300031008     C                   if        chkPRG = '0'
048400000706     C                   exsr      traduci
048500031008     C                   else
048600031008     C*
048700031008     C                   exsr      inztivlt
048800031008     C                   eval      vltsts = '2'
048900031008     C                   eval      vltmsg =
049000031008     C                               %trim(sysname)
049100031008     C                             + '/'
049200031008     C                             + %trim(psjobname)
049300031008     C                             + '/'
049400031008     C                             + %trim(psuser)
049500031008     C                             + '/'
049600031008     C                             + %trim(%editc(psjobnbr:'Z'))
049700031008     C                             + ' Progressivo/membro NON univoco!!!'
049800031008     C                   exsr      wrttivlt
049900031008     C                   eval      wrkesito = 'F'
050000031008     C                   exsr      sndeml
050100031008     C                   exsr      uscita
050200031008     C                   endif
050300991104     C*
050400991104     C                   endif
050500991104     C*
050600991104     C                   endsr
050700000622
050800000622
050900000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
051000000622     C     rtvtia        begsr
051100000622     C*
051200000622     C                   eval      tbecod = 'ISV'
051300000731     C                   eval      tbeke1 = virfi1
051400000622     C     keytbe        chain     tntbe01l
051500000622     C                   if        %found(tntbe01l)
051600020212     C                   movel     tbeuni        disv
051700000622     C                   endif
051800000622     C*
051900000622     C                   endsr
052000000706
052100000706
052200000706
052300000706      /TITLE Traduzione.
052400000706     C     traduci       begsr
052500031105     C*
052600031105     C* Reperisco informazioni membro da tradurre.
052700031105     C                   eval      mbrd04a = wrkfid
052800031105     C                   eval      mbrd05  = wrkmbd
052900031105     C                   exsr      rtvmbrd
053000031105     C* Controllo se il membro da tradurre ha dei record.
053100031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
053200031105     C                   if        (qusei=*blanks and
053300031105     C                             qusnbrcr > 0) or prmnfp > '01'
053400030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
053500030512     C                   call      'TIS7T9R2'
053600031008     C                   parm                    wrkfit
053700031008     C                   parm                    wrkmbt
053800030512     C                   parm      '0'           chkFTP            1
053900031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
054000031105     C                   if        chkFTP = '0'
054100000706     C*
054200000706     C                   exsr      inztivlt
054300000706     C* Chiamo il pgm traduttore.
054400000706     C                   call(e)   'TIS7T9C'
054500000706     C                   parm      wrkfid        prmfid
054600000706     C                   parm      wrkmbd        prmmbd
054700000706     C                   parm      wrkfis        prmfis
054800000706     C                   parm      wrkmbs        prmmbs
054900000706     C                   parm      wrkfit        prmfit
055000000706     C                   parm      wrkmbt        prmmbt
055100000706     C                   parm      wrkpgt        prmpgt
055200000706     C                   parm      wrkppt        prmppt
055300000706     C                   parm      *blanks       prmesito
055400000706     C* Se la call è finita in errore
055500000706     C* o la traduzione ha avuto degli errori.
055600000706     C                   if        %error
055700000712     C                             or prmesito <> '0'
055800000706     C                   eval      vltsts = '2'
055900000706     C                   eval      vltmsg =
056000000706     C                               %trim(sysname)
056100000706     C                             + '/'
056200000706     C                             + %trim(psjobname)
056300000706     C                             + '/'
056400000706     C                             + %trim(psuser)
056500000706     C                             + '/'
056600000706     C                             + %trim(%editc(psjobnbr:'Z'))
056700000706     C                             + ' Errori nella traduzione.'
056800000706     C                   else
056900000706     C                   eval      wrkesito = '0'
057000000706     C                   endif
057100000706     C*
057200000706     C                   exsr      wrttivlt
057300031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
057400031105     C                   else
057500030512     C                   exsr      inztivlt
057600030512     C                   eval      vltsts = '2'
057700030512     C                   eval      vltmsg =
057800030512     C                               %trim(sysname)
057900030512     C                             + '/'
058000030512     C                             + %trim(psjobname)
058100030512     C                             + '/'
058200030512     C                             + %trim(psuser)
058300030512     C                             + '/'
058400030512     C                             + %trim(%editc(psjobnbr:'Z'))
058500030512     C                             + ' Cliente/TipoFile in uso da server FTP'
058600030512     C                   exsr      wrttivlt
058700030512     C                   eval      wrkesito = 'E'
058800030512     C                   endif
058900031105     C* NO record da tradurre
059000031105     C                   endif
059100000706     C*
059200000706     C                   endsr
059300000328
059400000706
059500000706
059600000328      /TITLE Inizializzo variabili "wrk".
059700000328     C     inzwrk        begsr
059800000328     C*
059900000328     C                   clear                   wrkfit
060000000328     C                   clear                   wrkmbt
060100000328     C                   clear                   wrkpgi
060200000328     C                   clear                   wrkppi
060300000328     C                   clear                   wrkfis
060400000328     C                   clear                   wrkmbs
060500000328     C*
060600000328     C                   endsr
060700991026
060800000706
060900000706
061000991026      /TITLE Inizializzo TIVLT00F.
061100991026     C     inztivlt      begsr
061200000804     C*
061300000804     C                   exsr      rtvtia
061400991026     C*
061500000328     C                   clear                   tivlt000
061600991026     C                   eval      vltksc = wrkksc
061700991026     C                   eval      vltksu = wrkksu
061800000721     C                   eval      vlttip = wrktip
061900991026     C                   eval      vltfld = wrkfit
062000991026     C                   eval      vltmbd = wrkmbt
062100991026     C                   eval      vltpgi = wrkpgi
062200991026     C                   eval      vltppi = wrkppi
062300020212     C                   if        §isvproc = 'F'
062400000804     C                   eval      vltsnd = 'D'
062500000804     C                   else
062600991026     C                   eval      vltsnd = vtfsnd
062700000804     C                   endif
062800991026     C                   eval      vltfls = wrkfis
062900991026     C                   eval      vltmbs = wrkmbs
063000991026     C                   eval      vltdat = wrkoggi
063100991115     C                   eval      vltora = wrkora
063200991026     C                   eval      vltsts = '0'
063300991027     C                   eval      vltmsg =
063400991104     C                               %trim(sysname)
063500991104     C                             + '/'
063600991104     C                             + %trim(psjobname)
063700991027     C                             + '/'
063800991027     C                             + %trim(psuser)
063900991027     C                             + '/'
064000991027     C                             + %trim(%editc(psjobnbr:'Z'))
064100000328     C                   z-add     *zeros        vltsqz
064200000731     C*
064300991026     C                   endsr
064400991026
064500000706
064600000731
064700000731      /TITLE Inizializzo parzialmente TIVLT00F.
064800000731     C     inztivltp     begsr
064900000731     C*
065000000731     C                   clear                   tivlt000
065100000731     C                   eval      vltksc = wrkksc
065200000731     C                   eval      vltksu = wrkksu
065300000731     C                   eval      vlttip = wrktip
065400000731     C                   eval      vltpgi = wrkpgi
065500000731     C                   eval      vltppi = wrkppi
065600000731     C                   eval      vltsnd = vtfsnd
065700000731     C                   eval      vltdat = wrkoggi
065800000731     C                   eval      vltora = wrkora
065900000731     C                   eval      vltsts = '0'
066000000731     C                   eval      vltmsg =
066100000731     C                               %trim(sysname)
066200000731     C                             + '/'
066300000731     C                             + %trim(psjobname)
066400000731     C                             + '/'
066500000731     C                             + %trim(psuser)
066600000731     C                             + '/'
066700000731     C                             + %trim(%editc(psjobnbr:'Z'))
066800000731     C                   z-add     *zeros        vltsqz
066900000731     C*
067000000731     C                   exsr      rtvtia
067100000731     C*
067200000731     C                   endsr
067300000731
067400000731
067500000706
067600991026      /TITLE Scrittura TIVLT00F.
067700000731     C     wrttivlt      begsr
067800991026     C*
067900000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
068000000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
068100000731     C                   eval      vltisv = virfi1
068200020212     C                   eval      vlttia = §isvproc
068300000622     C                   eval      vltprg = dwlprg
068400000713     C                   eval      vltfl2 = prmnfp
068500991026     C                   write     tivlt000
068600040817     C*
068700050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
068800040817     C* procedura scambio dati
068900040817     C* ...quindi eseguo prima d tutto l'override
069000050909  "  C*
069100050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
069200050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
069300050909     C*                  eval      LUNG = %len(%trim(COMMAND))
069400050909  "  C*                  call(e)   'QCMDEXC'
069500050909     C*                  parm                    COMMAND
069600050909     C*                  parm                    LUNG             15 5
069700040817     C*
069800050909  "  C*                  if        not %error
069900050909     C*                  if        not %open(tivltnew)
070000050909     C*                  open(e)   tivltnew
070100050909     C*                  endif
070200050909  "  C*                  if        not %error
070300050909     C*                  write     tivltn00
070400050909     C*                  endif
070500050909     C*                  if        %open(tivltnew)
070600050909  "  C*                  close(e)  tivltnew
070700050909     C*                  endif
070800050909 TESTC*                  endif
070900991026     C*
071000991026     C                   endsr
071100991026
071200000706
071300000706
071400991026      /TITLE Operazioni finali.
071500991026     C     uscita        begsr
071600991026     C*
071700000627     C                   seton                                        lr
071800991026     C*
071900991026     C                   endsr
072000991026
072100000706
072200000706
072300991026      /TITLE Controllo formale parametri ricevuti.
072400991026     C     chkprm        begsr
072500991026     C*
072600991104     C                   if        wrkksu = *blanks
072700031008     C*
072800031008     C                   exsr      inztivlt
072900031008     C                   eval      vltsts = '2'
073000031008     C                   eval      vltmsg =
073100031008     C                               %trim(sysname)
073200031008     C                             + '/'
073300031008     C                             + %trim(psjobname)
073400031008     C                             + '/'
073500031008     C                             + %trim(psuser)
073600031008     C                             + '/'
073700031008     C                             + %trim(%editc(psjobnbr:'Z'))
073800031008     C                             + ' Parametro codice cliente non passato.'
073900031008     C                   exsr      wrttivlt
074000031008     C                   eval      wrkesito = 'A'
074100031008     C*
074200991026     C                   exsr      uscita
074300991026     C                   endif
074400991026     C*
074500991026     C                   if        wrktip = *blanks
074600031008     C*
074700031008     C                   exsr      inztivlt
074800031008     C                   eval      vltsts = '2'
074900031008     C                   eval      vltmsg =
075000031008     C                               %trim(sysname)
075100031008     C                             + '/'
075200031008     C                             + %trim(psjobname)
075300031008     C                             + '/'
075400031008     C                             + %trim(psuser)
075500031008     C                             + '/'
075600031008     C                             + %trim(%editc(psjobnbr:'Z'))
075700031008     C                             + ' Parametro tipo file non passato.'
075800031008     C                   exsr      wrttivlt
075900031008     C                   eval      wrkesito = 'B'
076000031008     C*
076100991026     C                   exsr      uscita
076200991026     C                   endif
076300991026     C*
076400991026     C                   clear                   wrkesito
076500991026     C*
076600991026     C                   endsr
076700991111
076800000706
076900000706
077000991111      /TITLE Reperisco informazioni membro.
077100991111     C     rtvmbrd       begsr
077200991111     C* Pulisco e imposto la DS degli errori.
077300000124     C                   clear                   qusec
077400991111     C                   eval      qusbprv = %size(qusec)
077500991111     C*
077600991111     C                   call      'QUSRMBRD'
077700991111     C     qusm0200      parm                    mbrd01
077800991111     C                   parm                    mbrd02
077900991111     C                   parm                    mbrd03
078000991111     C                   parm                    mbrd04
078100991111     C                   parm                    mbrd05
078200991111     C                   parm                    mbrd06
078300991111     C                   parm                    qusec
078400991111     C*
078500991111     C                   endsr
078600031008
078700031008
078800031008
078900031008
079000031008      /TITLE Compongo il testo e spedisco una e-m@ail
079100031008     C     sndeml        begsr
079200031008     C*
079300031008     C* Inizializzo variabili
079400031008     C                   movel     *blanks       wrkEml          253
079500031008     C                   movel     *blanks       wrkEmlMsg      5000
079600031008     C                   movel     *blanks       wrkEmlOgg        44
079700031008     C* Valorizzo i campi della e-m@ail
079800120305     C                   eval      wrkEml='cedalert@brt.it'
079900031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
080000031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
080100031008     C                             'SU AS/400'                         +':/N'+
080200031008     C                             ' '                                 +':/N'+
080300031008     C                             'RIFERIMENTI'                       +':/N'+
080400031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
080500031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
080600031008     C                             'Cliente:'+vltksc                   +':/N'+
080700031008     C                             'TipoFile:'+vlttip                  +':/N'+
080800031008     C                             ' '                                 +':/N'+
080900031008     C                             'MESSAGGIO'                         +':/N'+
081000031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
081100031008     C*
081200031008     C                   call(e)   'TIS701C'
081300031008     C                   parm                    wrkEml
081400031008     C                   parm                    wrkEmlOgg
081500031008     C                   parm                    wrkEmlMsg
081600031008     C*
081700031008     C                   endsr
081800990910
081900000706
082000000706
082100990908      /TITLE Operazioni iniziali.
082200990908     C     *inzsr        begsr
082300990908     C*
082400990908     C     *entry        plist
082500991026     C     wrkksc        parm                    prmksc
082600991026     C     wrkksu        parm      wrkksu        prmksu
082700991026     C     wrktip        parm                    prmtip
082800991026     C                   parm      wrkesito      prmesito
082900000623     C                   parm                    prmprog
083000000623     C                   parm                    prmsrv
083100000628     C                   parm                    prmope
083200000713     C                   parm                    prmnfp
083300991104     C*
083400991105     C     k02vss02      klist
083500991105     C                   kfld                    vssksu
083600991104     C                   kfld                    vssisv
083700991026     C*
083800991026     C     k03vir02      klist
083900991026     C                   kfld                    virksc
084000991026     C                   kfld                    virtip
084100991026     C                   kfld                    virdti
084200991026     C*
084300991026     C     k02vir02      klist
084400991026     C                   kfld                    virksc
084500991026     C                   kfld                    virtip
084600000706     C*
084700000706     C     k04vir02      klist
084800000706     C                   kfld                    vreksc
084900000706     C                   kfld                    vretfl
085000000706     C                   kfld                    vredti
085100990920     C*
085200990920     C     k01vtf01      klist
085300990920     C                   kfld                    vtftip
085400000622     C*
085500000622     C     keytbe        klist
085600000622     C                   kfld                    tbecod
085700000622     C                   kfld                    tbeke1
085800000712     C*
085900000712     C     k01vlt        klist
086000000712     C                   kfld                    vltprg
086100000712     C                   kfld                    vltksc
086200031008     C*
086300031008     C     k08vlt        klist
086400031008     C                   kfld                    wrkfit
086500031008     C                   kfld                    wrkmbt
086600000706     C*
086700991104     C* Reperisco nome sistema.
086800991104     C                   eval      qusbprv = %size(qusec)
086900991104     C                   call      'QWCRNETA'
087000991104     C                   parm                    rneta01
087100991104     C                   parm                    rneta02
087200991104     C                   parm                    rneta03
087300991104     C                   parm                    rneta04
087400991104     C                   parm                    qusec
087500991104     C                   eval      nait = rneta01c
087600990908     C*
087700990908     C                   endsr
087800991026** ctdata mc
087900991026ABCDEFGHILMN
