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
004400070207     D prmout          s             10
004500991026     D wrkksc          s                   like(prmksc)
004600991026     D wrkksu          s                   like(prmksu)
004700991026     D wrktip          s                   like(prmtip)
004800991026     D wrkesito        s                   like(prmesito)
004900991026     D wrkoggiiso      s               d
005000991026     D wrkoggi         s              8  0
005100991026     D wrkora          s              6  0
005200991026     D wrkmese         s              2  0
005300000623     D wrkprg          s              8  0 INZ(*zeros)
005400000711     D wrkvarie        s              1    INZ('0')
005500000712     D wrkmsg          s             80    INZ(*blanks)
005600000914     D vssok           s               n   INZ(*off)
005700051011
005800051011     D dwlprg          ds
005900051011     D  dwl_isv                       2    INZ(*all'0')
006000051207     D**dwl_tip                       2    INZ(*all'0')
006100051207     D**dwl_prg                       6    INZ(*all'0')
006200051207     D  dwl_prg                       8    INZ(*all'0')
006300000711
006400991026     D                 ds                  inz
006500991026     D wrkday                  1      2s 0
006600991026     D wrkdaychr                      2    overlay(wrkday)
006700991026     D wrkpgt          s                   like(vtfpgt)
006800991026     D wrkppt          s                   like(vtfppt)
006900000711     D wrkpgi          s                   like(vtfpgi)
007000000711     D wrkppi          s                   like(vtfppi)
007100991026     D wrkfid          s                   like(vtffid)
007200991026     D wrkmbd          s                   like(vltmbd)
007300991026     D wrkfis          s                   like(vtffis)
007400991026     D wrkmbs          s                   like(vltmbs)
007500991026     D wrkfit          s                   like(vtffit)
007600991026     D wrkmbt          s                   like(vltmbd)
007700991026     D prmfid          s                   like(wrkfid)
007800991026     D prmmbd          s                   like(wrkmbd)
007900991026     D prmfis          s                   like(wrkfis)
008000991026     D prmmbs          s                   like(wrkmbs)
008100991026     D prmfit          s                   like(wrkfit)
008200991027     D prmmbt          s                   like(wrkmbt)
008300991026     D prmpgt          s                   like(wrkpgt)
008400991026     D prmppt          s                   like(wrkppt)
008500991026     D mc              s              1    dim(12) ctdata perrcd(12)
008600991104
008700991111     D mbrd01          s            266
008800991111     D mbrd02          s              8b 0 inz(%size(mbrd01))
008900991111     D mbrd03          s              8    inz('MBRD0200')
009000991111     D mbrd04          ds
009100991111     D mbrd04a                       10                                         File
009200991111     D mbrd04b                       10    inz('*LIBL     ')                    Libreria
009300991111     D mbrd05          s             10                                         Membro
009400991125     D mbrd06          s              1    inz('0')
009500991111     D/COPY QSYSINC/QRPGLESRC,QUSRMBRD
009600991104     D rneta01         ds
009700991104     D  rneta01a                      8b 0
009800991104     D  rneta01b                      8b 0
009900991104     D  rneta01c                     24
010000991104     D rneta02         s              8b 0 inz(%size(rneta01))
010100991104     D rneta03         s              8b 0 inz(1)
010200991104     D rneta04         s             10    inz('SYSNAME')
010300991104     D nait            ds
010400991104     D  nait01                       10
010500991104     D  nait02                        1
010600991104     D  nait03                        1
010700991104     D  nait04                        8b 0
010800991104     D  sysname                       8
010900991111     D/COPY QSYSINC/QRPGLESRC,QUSEC
011000991104
011100000622
011200000905     D*-------------------
011300000905     D* DS di ridefinizione file di log x passaggio recordset SQL
011400000905     D*-------------------
011500000905     D  dsvlt        E DS                  extname(tivlt00f)
011600000905     D                                     prefix(s_)
011700000905
011800040817
011900040817     D*-------------------
012000050909 TESTD* Variabili d wrk
012100040817     D*-------------------
012200050909     D*  COMMAND        S            300    inz(*blanks)
012300040817
012400000905
012500000905
012600000622      /TITLE *** MAIN ***
012700000622     C*
012800000906     C                   if        prmnfp  > '01' and
012900000906     C                             prmprog > *blanks
013000000905     C                   exsr      rtvdatora
013100000905     C                   else
013200000905     C                   time                    wrkoggiiso
013300000905     C                   time                    wrkora
013400000905     C     *iso          move      wrkoggiiso    wrkoggi
013500000905     C                   endif
013600030519     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
013700000328     C* Inizializzo variabili "wrk".
013800000328     C                   exsr      inzwrk
013900991026     C* Controllo parametri ricevuti.
014000991026     C                   exsr      chkprm
014100000626     C* Reperisco informazioni file.
014200000706     C                   exsr      rtvleg
014300000706     C* Alla fine...
014400000706     C                   exsr      uscita
014500000626
014600000626
014700000905
014800000905      /TITLE Routine di reperimento data/ora da log x operazioni "multi-file"
014900000905     C*
015000000905     C     rtvdatora     begsr
015100000905     C*
015200000905     C/EXEC SQL
015300000905     C+ declare C1 cursor for select *
015400000905     C+ from tivlt00f where
015500000905     C+ vltprg = :prmprog
015600000905     C+ for fetch only
015700000905     C/END-EXEC
015800000905     C
015900000905     C/EXEC SQL
016000000905     C+ open C1
016100000905     C/END-EXEC
016200000905     C
016300000905     C/EXEC SQL
016400000905     C+ Fetch C1 into :dsvlt
016500000905     C/END-EXEC
016600000905     C*
016700000906     C                   if        sqlcod = *zeros     and
016800000906     C                             s_vltprg  = prmprog and
016900000906     C                             s_vltdat  > *zeros  and
017000000905     C                             s_vltora  > *zeros
017100000905     C                   z-add     s_vltdat      wrkoggi
017200000905     C                   z-add     s_vltora      wrkora
017300000905     C                   else
017400000905     C                   time                    wrkoggiiso
017500000905     C                   time                    wrkora
017600000905     C     *iso          move      wrkoggiiso    wrkoggi
017700000905     C                   endif
017800000905     C
017900000905     C/EXEC SQL
018000000905     C+ close C1
018100000905     C/END-EXEC
018200000905     C
018300000905     C*
018400000905     C                   endsr
018500000905
018600000905
018700000706
018800000706      /TITLE Determino eventuali legami cliente/tipi file
018900000706     C     rtvleg        begsr
019000000706     C*
019100000706     C                   eval      virksc = wrkksu
019200000706     C                   eval      virtip = wrktip
019300000706     C                   eval      virdti = wrkoggi
019400000707     C                   eval      vtftip = wrktip
019500000706     C* Reperisco informazioni file.
019600000706     C                   exsr      rtvtip
019700000706     C*
019800000713     C* Se almeno per il tipo file "master" è ok e trattasi unicamente di "primo lancio std"
019900000713     C                   if        wrkesito = '0'     and
020000000713     C                             prmprog  = *blanks and
020100000713     C                             prmope   = *blanks
020200000711     C* Cambio il valore del flag <wrkvarie> per "sapere" che stò elaborando i legami tipi file
020300000711     C                   movel     '1'           wrkvarie
020400000706     C*
020500000706     C* Verifico ed eventualmente ciclo sui legami cliente/tipi file
020600000707     C     k03vir02      setll     tivre01l
020700000707     C                   if        %found(tivre01l)
020800000707     C     k03vir02      reade     tivre01l
020900000706     C                   dow       not %eof
021000000706     C                   eval      virksc = wrkksu
021100000706     C                   eval      virtip = vretfl
021200000706     C                   eval      virdti = wrkoggi
021300000707     C                   eval      vtftip = vretfl
021400000712     C                   eval      wrktip = vretfl
021500000706     C* Reperisco informazioni file.
021600000706     C                   exsr      rtvtip
021700000712     C                   eval      virtip = vretip
021800000707     C     k03vir02      reade     tivre01l
021900000706     C                   enddo
022000000706     C                   endif
022100000706     C*
022200000706     C                   endif
022300031008     C* Eseguo a prescindere dal risultato il PGM "post-traduzione" (solo se nn allocato
022400031008     C* dalla procedura FTP/e-mail) e solo se il progressivo è univoco
022500031008     C                   if        chkPRG <> '1'
022600030512     C                   if        chkFTP <> '1'
022700000711     C                   exsr      exepgm
022800030512     C                   endif
022900031008     C                   endif
023000000706     C*
023100000706     C                   endsr
023200000706
023300000706
023400000706
023500000706      /TITLE Reperisco tipo file.
023600000706     C     rtvtip        begsr
023700000706     C* Reperisco il tipo file.
023800000706     C     k01vtf01      chain     tivtf01l
023900000706     C* Se il tipo file non esiste
024000000706     C* oppure non è valido per il download.
024100000706     C                   if        not %found
024200000706     C                             or vtfud <> 'D'
024300031008     C*
024400031008     C                   exsr      inztivlt
024500031008     C                   eval      vltsts = '2'
024600031008     C                   eval      vltmsg =
024700031008     C                               %trim(sysname)
024800031008     C                             + '/'
024900031008     C                             + %trim(psjobname)
025000031008     C                             + '/'
025100031008     C                             + %trim(psuser)
025200031008     C                             + '/'
025300031008     C                             + %trim(%editc(psjobnbr:'Z'))
025400031008     C                             + ' Tipo file inesistente.'
025500031008     C                   exsr      wrttivlt
025600031008     C                   eval      wrkesito = 'B'
025700031008     C*
025800000721     C                   else
025900000706     C* Reperisco il tipo file del cliente.
026000000706     C     k03vir02      setgt     tivir02l
026100000706     C     k02vir02      readpe    tivir02l
026200000706     C* Tipo file non previsto per il cliente
026300000706     C* oppure scaduto.
026400000731     C                   if        %eof
026500000706     C                             or virdtf < wrkoggi
026600000706     C                             or virdti > wrkoggi
026700000706     C*
026800000731     C                   exsr      inztivltp
026900000706     C*
027000000706     C                   eval      vltsts = '2'
027100000706     C                   eval      vltmsg =
027200000706     C                               %trim(sysname)
027300000706     C                             + '/'
027400000706     C                             + %trim(psjobname)
027500000706     C                             + '/'
027600000706     C                             + %trim(psuser)
027700000706     C                             + '/'
027800000706     C                             + %trim(%editc(psjobnbr:'Z'))
027900000706     C                             + ' Tipo file non previsto per il cliente.'
028000000706     C                   exsr      wrttivlt
028100000706     C                   eval      wrkesito = 'C'
028200000706     C*
028300000706     C                   else
028400000706     C*
028500000706     C* Imposto i default.
028600000706     C                   if        virpgt <> *blanks
028700000706     C                   eval      wrkpgt = virpgt
028800000706     C                   eval      wrkppt = virppt
028900000706     C                   else
029000000706     C                   eval      wrkpgt = vtfpgt
029100000706     C                   eval      wrkppt = vtfppt
029200000706     C                   endif
029300000712     C*
029400000711     C* Le istruzioni comprese nel blocco marcato le effettuo solo sul giro del tipo file "master"
029500000711     C                   if        wrkvarie = '0'                               <===================
029600000711     C                   if        virpgi <> *blanks                            <===================
029700000711     C                   eval      wrkpgi = virpgi                              <===================
029800000711     C                   eval      wrkppi = virppi                              <===================
029900000711     C                   else                                                   <===================
030000000711     C                   eval      wrkpgi = vtfpgi                              <===================
030100000711     C                   eval      wrkppi = vtfppi                              <===================
030200000711     C                   endif                                                  <===================
030300000711     C                   endif                                                  <===================
030400000706     C* File da tradurre.
030500000706     C                   if        virfid <> *blanks
030600000706     C                   eval      wrkfid = virfid
030700000706     C                   else
030800000706     C                   eval      wrkfid = vtffid
030900000706     C                   endif
031000000706     C                   eval      wrkmbd = 'M' + %subst(wrkksu:2:7)
031100000706     C* File storico.
031200000706     C                   if        virfis <> *blanks
031300000706     C                   eval      wrkfis = virfis
031400000706     C                   else
031500000706     C                   eval      wrkfis = vtffis
031600000706     C                   endif
031700000706     C                   extrct    wrkoggiiso:*m wrkmese
031800000706     C                   extrct    wrkoggiiso:*d wrkday
031900000706     C                   eval      wrkmbs =
032000000706     C                             mc(wrkmese)
032100000706     C                             + wrkdaychr
032200000706     C                             + %subst(wrkksu:2:7)
032300030519     C*
032400030519     C                   eval      vssisv = virfi1
032500030519     C* Valorizzo subito il progressivo applicazione.
032600030519     C                   exsr      calprog
032700000706     C* File tradotto.
032800000706     C                   if        virfit <> *blanks
032900000706     C                   eval      wrkfit = virfit
033000000706     C                   else
033100000706     C                   eval      wrkfit = vtffit
033200000706     C                   endif
033300030526     C* Tipo nome membro tradotto
033400030526     C                   eval      dvir01 = virpth
033500070207     C* Se forzatura in ingresso adotto quella, altrimenti applico considerazioni standard
033600070207     C                   if        prmout <> *blanks
033700070207     C                   eval      wrkmbt = prmout                              membro = forzatura
033800070207     C                   else
033900030526     C* A seconda del parametro che indica il tipo di nomemclatura da dare al membri tradotto
034000030526     C                   if        §VIR01TM = 'P'
034100030526     C                   eval      wrkmbt = dwlprg                              membro = progressivo
034200030526 xxx C***                eval      wrkmbs = dwlprg
034300030519     C                   else
034400030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
034500070207     C                   endif
034600030519     C                   endif
034700000706     C* Controllo abilitazione al servizio.
034800000706     C                   exsr      chkvss
034900000706     C                   endif
035000000706     C                   endif
035100000706     C*
035200000706     C                   endsr
035300000706
035400000711
035500000711
035600000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
035700000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
035800000711     C*
035900000711     C     exepgm        begsr
036000000711     C* Lancio il pgm in variabile con relativi parametri
036100000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
036200000711    C                   if        wrkpgi <> *blanks
036300000712     C* Chiamo il pgm post traduzione.
036400000712     C                   call(e)   'TIS7T9C1'
036500000712     C                   parm      wrkfid        prmfid
036600000712     C                   parm      wrkmbd        prmmbd
036700000712     C                   parm      wrkfis        prmfis
036800000712     C                   parm      wrkmbs        prmmbs
036900031106     C                   parm      wrkfit        prmfit
037000031106     C                   parm      wrkmbt        prmmbt
037100000712     C                   parm                    wrkpgi
037200000712     C                   parm                    wrkppi
037300000712     C                   parm      *blanks       prmesito
037400000712     C* Se la call è finita in errore
037500000712     C* o il lancio del PGM ha avuto degli errori.
037600000712     C                   if        %error
037700000712     C                             or prmesito <> '0'
037800000712     C     k01vlt        setll     tivlt07l
037900000712     C                   if        %found(tivlt07l)
038000000712     C     k01vlt        reade     tivlt07l
038100000712     C                   dow       not %eof
038200000712     C                   if        vltsqz = *zeros and
038300000712     C                             (vltsts = '0' or vltsts = '1')
038400000712     C                   eval      vltsts = '2'
038500000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
038600000712     C                   eval      vltmsg =
038700000712     C                               %trim(sysname)
038800000712     C                             + '/'
038900000712     C                             + %trim(psjobname)
039000000712     C                             + '/'
039100000712     C                             + %trim(psuser)
039200000712     C                             + '/'
039300000712     C                             + %trim(%editc(psjobnbr:'Z'))
039400000712     C                             + %trim(wrkmsg)
039500000712     C                   update    tivlt700
039600000712     C                   endif
039700000712     C     k01vlt        reade     tivlt07l
039800000712     C                   enddo
039900000712     C                   endif
040000000712     C                   endif
040100000712     C*
040200000711     C                   endif
040300000711     C*
040400000711     C                   endsr
040500000711
040600000711
040700000626
040800000626      /TITLE Valorizzazione Progressivo Applicazione
040900000626     C     calprog       begsr
041000000626     C*
041100000706     C                   if        dwlprg = *all'0'
041200000627     C                   read(e)   tis7prgf
041300000627     C                   if        not %error
041400000627     C                   eval      dwlprg = f_tis7prgf
041500000627     C*
041600000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
041700000719     C                   if        prmsrv <> *blanks
041800000719     C                   eval      vssisv = prmsrv
041900000719     C                   endif
042000000626     C*
042100000719     C                   if        prmprog = *blanks
042200051011     C                   move(p)   dwl_prg       wrkprg
042300000626     C                   add       1             wrkprg
042400051011     C                   move(p)   wrkprg        dwl_prg
042500051011     C                   movel     vssisv        dwl_isv
042600051207     C*                  if        wrktip <> *blanks
042700051207     C*                  movel     wrktip        dwl_tip
042800051207     C*                  else
042900051207     C*                  movel     'XX'          dwl_tip
043000051207     C*                  endif
043100040123     C                   eval      f_tis7prgf = dwlprg
043200040123     C                   update    tis7prg0
043300000626     C                   else
043400040123     C                   unlock(e) tis7prgf
043500000626     C                   eval      dwlprg = prmprog
043600000626     C                   endif
043700000627     C*
043800000627     C                   endif
043900000706     C                   endif
044000000626     C*
044100000626     C                   endsr
044200991104
044300000706
044400000706
044500000706
044600991104      /TITLE Controllo abilitazione servizio.
044700991104     C     chkvss        begsr
044800991104     C*
044900000914     C                   eval      vssok = *off
045000000914     C*
045100991105     C                   eval      vssksu = wrkksu
045200000914     C     k02vss02      setll     tivss02l
045300000914     C     k02vss02      reade     tivss02l
045400000914     C                   dow       not %eof
045500000914     C                   if        vssdde <= wrkoggi and
045600000914     C                             vssdsc >= wrkoggi and
045700000914     C                             vssvat <> 'A'     and
045800000914     C                             vssvat <> 'S'
045900000914     C                   eval      vssok = *on
046000000914     C                   leave
046100000914     C                   endif
046200000914     C     k02vss02      reade     tivss02l
046300000914     C                   enddo
046400000914     C*
046500000914     C                   if        vssok = *off
046600991104     C*
046700000622     C                   exsr      inztivlt
046800000329     C                   eval      vltsts = '2'
046900991104     C                   eval      vltmsg =
047000991104     C                               %trim(sysname)
047100991104     C                             + '/'
047200991104     C                             + %trim(psjobname)
047300991104     C                             + '/'
047400991104     C                             + %trim(psuser)
047500991104     C                             + '/'
047600991104     C                             + %trim(%editc(psjobnbr:'Z'))
047700991104     C                             + ' Cliente non abilitato al servizio.'
047800000622     C                   exsr      wrttivlt
047900000622     C                   eval      wrkesito = 'D'
048000031008     C*
048100000622     C                   else
048200031008     C*
048300031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
048400031008     C* del progressivo rispetto a file/membro e cliente
048500031008     C                   movel     '0'           chkPRG            1
048600031008     C     k08vlt        setll     tivlt08l
048700031008     C                   if        %equal(tivlt08l)
048800031008     C     k08vlt        reade     tivlt08l
048900031008     C                   dow       not %eof(tivlt08l)
049000031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
049100031008     C                   if        w_vltksc <> wrkksc
049200031008     C                   movel     '1'           chkPRG
049300031008     C                   endif
049400031008     C     k08vlt        reade     tivlt08l
049500031008     C                   enddo
049600031008     C                   endif
049700031008     C*
049800000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
049900031008     C                   if        chkPRG = '0'
050000000706     C                   exsr      traduci
050100031008     C                   else
050200031008     C*
050300031008     C                   exsr      inztivlt
050400031008     C                   eval      vltsts = '2'
050500031008     C                   eval      vltmsg =
050600031008     C                               %trim(sysname)
050700031008     C                             + '/'
050800031008     C                             + %trim(psjobname)
050900031008     C                             + '/'
051000031008     C                             + %trim(psuser)
051100031008     C                             + '/'
051200031008     C                             + %trim(%editc(psjobnbr:'Z'))
051300031008     C                             + ' Progressivo/membro NON univoco!!!'
051400031008     C                   exsr      wrttivlt
051500031008     C                   eval      wrkesito = 'F'
051600031008     C                   exsr      sndeml
051700031008     C                   exsr      uscita
051800031008     C                   endif
051900991104     C*
052000991104     C                   endif
052100991104     C*
052200991104     C                   endsr
052300000622
052400000622
052500000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
052600000622     C     rtvtia        begsr
052700000622     C*
052800000622     C                   eval      tbecod = 'ISV'
052900000731     C                   eval      tbeke1 = virfi1
053000000622     C     keytbe        chain     tntbe01l
053100000622     C                   if        %found(tntbe01l)
053200020212     C                   movel     tbeuni        disv
053300000622     C                   endif
053400000622     C*
053500000622     C                   endsr
053600000706
053700000706
053800000706
053900000706      /TITLE Traduzione.
054000000706     C     traduci       begsr
054100031105     C*
054200031105     C* Reperisco informazioni membro da tradurre.
054300031105     C                   eval      mbrd04a = wrkfid
054400031105     C                   eval      mbrd05  = wrkmbd
054500031105     C                   exsr      rtvmbrd
054600031105     C* Controllo se il membro da tradurre ha dei record.
054700031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
054800031105     C                   if        (qusei=*blanks and
054900060616     C                             qusnbrcr > 0) or prmnfp > '01'
055000060616     C                             or %trim(wrkfid) = '*'
055100030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
055200030512     C                   call      'TIS7T9R2'
055300031008     C                   parm                    wrkfit
055400031008     C                   parm                    wrkmbt
055500030512     C                   parm      '0'           chkFTP            1
055600031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
055700031105     C                   if        chkFTP = '0'
055800000706     C*
055900000706     C                   exsr      inztivlt
056000000706     C* Chiamo il pgm traduttore.
056100060616     C                   if        %trim(wrkfid) = '*' AND
056200060616     C                             %trim(wrkfis) = '*'
056300060616     C                   call(e)   'TIS7T9C3'
056400060616     C                   parm      wrkmbd        prmmbd
056500060616     C                   parm      wrkfit        prmfit
056600060616     C                   parm      wrkmbt        prmmbt
056700060616     C                   parm      wrkpgt        prmpgt
056800060616     C                   parm      wrkppt        prmppt
056900060616     C                   parm      *blanks       prmesito
057000060616     C                   else
057100000706     C                   call(e)   'TIS7T9C'
057200000706     C                   parm      wrkfid        prmfid
057300000706     C                   parm      wrkmbd        prmmbd
057400000706     C                   parm      wrkfis        prmfis
057500000706     C                   parm      wrkmbs        prmmbs
057600000706     C                   parm      wrkfit        prmfit
057700000706     C                   parm      wrkmbt        prmmbt
057800000706     C                   parm      wrkpgt        prmpgt
057900000706     C                   parm      wrkppt        prmppt
058000000706     C                   parm      *blanks       prmesito
058100060616     C                   endif
058200000706     C* Se la call è finita in errore
058300000706     C* o la traduzione ha avuto degli errori.
058400060320     C                   if        %error
058500060320     C                             or (prmesito <> '0' and
058600060320     C                                 prmesito <> 'Y')
058700000706     C                   eval      vltsts = '2'
058800000706     C                   eval      vltmsg =
058900000706     C                               %trim(sysname)
059000000706     C                             + '/'
059100000706     C                             + %trim(psjobname)
059200000706     C                             + '/'
059300000706     C                             + %trim(psuser)
059400000706     C                             + '/'
059500000706     C                             + %trim(%editc(psjobnbr:'Z'))
059600000706     C                             + ' Errori nella traduzione.'
059700000706     C                   else
059800060320     C                   if        prmesito = 'Y'
059900060320     C                   eval      wrkesito = prmesito
060000060320     C                   else
060100000706     C                   eval      wrkesito = '0'
060200060320     C                   endif
060300000706     C                   endif
060400000706     C*
060500000706     C                   exsr      wrttivlt
060600031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
060700031105     C                   else
060800030512     C                   exsr      inztivlt
060900030512     C                   eval      vltsts = '2'
061000030512     C                   eval      vltmsg =
061100030512     C                               %trim(sysname)
061200030512     C                             + '/'
061300030512     C                             + %trim(psjobname)
061400030512     C                             + '/'
061500030512     C                             + %trim(psuser)
061600030512     C                             + '/'
061700030512     C                             + %trim(%editc(psjobnbr:'Z'))
061800030512     C                             + ' Cliente/TipoFile in uso da server FTP'
061900030512     C                   exsr      wrttivlt
062000030512     C                   eval      wrkesito = 'E'
062100030512     C                   endif
062200031105     C* NO record da tradurre
062300031105     C                   endif
062400000706     C*
062500000706     C                   endsr
062600000328
062700000706
062800000706
062900000328      /TITLE Inizializzo variabili "wrk".
063000000328     C     inzwrk        begsr
063100000328     C*
063200000328     C                   clear                   wrkfit
063300000328     C                   clear                   wrkmbt
063400000328     C                   clear                   wrkpgi
063500000328     C                   clear                   wrkppi
063600000328     C                   clear                   wrkfis
063700000328     C                   clear                   wrkmbs
063800000328     C*
063900000328     C                   endsr
064000991026
064100000706
064200000706
064300991026      /TITLE Inizializzo TIVLT00F.
064400991026     C     inztivlt      begsr
064500000804     C*
064600000804     C                   exsr      rtvtia
064700991026     C*
064800000328     C                   clear                   tivlt000
064900991026     C                   eval      vltksc = wrkksc
065000991026     C                   eval      vltksu = wrkksu
065100000721     C                   eval      vlttip = wrktip
065200991026     C                   eval      vltfld = wrkfit
065300991026     C                   eval      vltmbd = wrkmbt
065400991026     C                   eval      vltpgi = wrkpgi
065500991026     C                   eval      vltppi = wrkppi
065600020212     C                   if        §isvproc = 'F'
065700000804     C                   eval      vltsnd = 'D'
065800000804     C                   else
065900991026     C                   eval      vltsnd = vtfsnd
066000000804     C                   endif
066100991026     C                   eval      vltfls = wrkfis
066200991026     C                   eval      vltmbs = wrkmbs
066300991026     C                   eval      vltdat = wrkoggi
066400991115     C                   eval      vltora = wrkora
066500991026     C                   eval      vltsts = '0'
066600991027     C                   eval      vltmsg =
066700991104     C                               %trim(sysname)
066800991104     C                             + '/'
066900991104     C                             + %trim(psjobname)
067000991027     C                             + '/'
067100991027     C                             + %trim(psuser)
067200991027     C                             + '/'
067300991027     C                             + %trim(%editc(psjobnbr:'Z'))
067400000328     C                   z-add     *zeros        vltsqz
067500000731     C*
067600991026     C                   endsr
067700991026
067800000706
067900000731
068000000731      /TITLE Inizializzo parzialmente TIVLT00F.
068100000731     C     inztivltp     begsr
068200000731     C*
068300000731     C                   clear                   tivlt000
068400000731     C                   eval      vltksc = wrkksc
068500000731     C                   eval      vltksu = wrkksu
068600000731     C                   eval      vlttip = wrktip
068700000731     C                   eval      vltpgi = wrkpgi
068800000731     C                   eval      vltppi = wrkppi
068900000731     C                   eval      vltsnd = vtfsnd
069000000731     C                   eval      vltdat = wrkoggi
069100000731     C                   eval      vltora = wrkora
069200000731     C                   eval      vltsts = '0'
069300000731     C                   eval      vltmsg =
069400000731     C                               %trim(sysname)
069500000731     C                             + '/'
069600000731     C                             + %trim(psjobname)
069700000731     C                             + '/'
069800000731     C                             + %trim(psuser)
069900000731     C                             + '/'
070000000731     C                             + %trim(%editc(psjobnbr:'Z'))
070100000731     C                   z-add     *zeros        vltsqz
070200000731     C*
070300000731     C                   exsr      rtvtia
070400000731     C*
070500000731     C                   endsr
070600000731
070700000731
070800000706
070900991026      /TITLE Scrittura TIVLT00F.
071000000731     C     wrttivlt      begsr
071100991026     C*
071200000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
071300000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
071400000731     C                   eval      vltisv = virfi1
071500020212     C                   eval      vlttia = §isvproc
071600000622     C                   eval      vltprg = dwlprg
071700000713     C                   eval      vltfl2 = prmnfp
071800991026     C                   write     tivlt000
071900040817     C*
072000050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
072100040817     C* procedura scambio dati
072200040817     C* ...quindi eseguo prima d tutto l'override
072300050909  "  C*
072400050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
072500050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
072600050909     C*                  eval      LUNG = %len(%trim(COMMAND))
072700050909  "  C*                  call(e)   'QCMDEXC'
072800050909     C*                  parm                    COMMAND
072900050909     C*                  parm                    LUNG             15 5
073000040817     C*
073100050909  "  C*                  if        not %error
073200050909     C*                  if        not %open(tivltnew)
073300050909     C*                  open(e)   tivltnew
073400050909     C*                  endif
073500050909  "  C*                  if        not %error
073600050909     C*                  write     tivltn00
073700050909     C*                  endif
073800050909     C*                  if        %open(tivltnew)
073900050909  "  C*                  close(e)  tivltnew
074000050909     C*                  endif
074100050909 TESTC*                  endif
074200991026     C*
074300991026     C                   endsr
074400991026
074500000706
074600000706
074700991026      /TITLE Operazioni finali.
074800991026     C     uscita        begsr
074900991026     C*
075000000627     C                   seton                                        lr
075100991026     C*
075200991026     C                   endsr
075300991026
075400000706
075500000706
075600991026      /TITLE Controllo formale parametri ricevuti.
075700991026     C     chkprm        begsr
075800991026     C*
075900991104     C                   if        wrkksu = *blanks
076000031008     C*
076100031008     C                   exsr      inztivlt
076200031008     C                   eval      vltsts = '2'
076300031008     C                   eval      vltmsg =
076400031008     C                               %trim(sysname)
076500031008     C                             + '/'
076600031008     C                             + %trim(psjobname)
076700031008     C                             + '/'
076800031008     C                             + %trim(psuser)
076900031008     C                             + '/'
077000031008     C                             + %trim(%editc(psjobnbr:'Z'))
077100031008     C                             + ' Parametro codice cliente non passato.'
077200031008     C                   exsr      wrttivlt
077300031008     C                   eval      wrkesito = 'A'
077400031008     C*
077500991026     C                   exsr      uscita
077600991026     C                   endif
077700991026     C*
077800991026     C                   if        wrktip = *blanks
077900031008     C*
078000031008     C                   exsr      inztivlt
078100031008     C                   eval      vltsts = '2'
078200031008     C                   eval      vltmsg =
078300031008     C                               %trim(sysname)
078400031008     C                             + '/'
078500031008     C                             + %trim(psjobname)
078600031008     C                             + '/'
078700031008     C                             + %trim(psuser)
078800031008     C                             + '/'
078900031008     C                             + %trim(%editc(psjobnbr:'Z'))
079000031008     C                             + ' Parametro tipo file non passato.'
079100031008     C                   exsr      wrttivlt
079200031008     C                   eval      wrkesito = 'B'
079300031008     C*
079400991026     C                   exsr      uscita
079500991026     C                   endif
079600991026     C*
079700991026     C                   clear                   wrkesito
079800991026     C*
079900991026     C                   endsr
080000991111
080100000706
080200000706
080300991111      /TITLE Reperisco informazioni membro.
080400991111     C     rtvmbrd       begsr
080500991111     C* Pulisco e imposto la DS degli errori.
080600000124     C                   clear                   qusec
080700991111     C                   eval      qusbprv = %size(qusec)
080800991111     C*
080900991111     C                   call      'QUSRMBRD'
081000991111     C     qusm0200      parm                    mbrd01
081100991111     C                   parm                    mbrd02
081200991111     C                   parm                    mbrd03
081300991111     C                   parm                    mbrd04
081400991111     C                   parm                    mbrd05
081500991111     C                   parm                    mbrd06
081600991111     C                   parm                    qusec
081700991111     C*
081800991111     C                   endsr
081900031008
082000031008
082100031008
082200031008
082300031008      /TITLE Compongo il testo e spedisco una e-m@ail
082400031008     C     sndeml        begsr
082500031008     C*
082600031008     C* Inizializzo variabili
082700031008     C                   movel     *blanks       wrkEml          253
082800031008     C                   movel     *blanks       wrkEmlMsg      5000
082900031008     C                   movel     *blanks       wrkEmlOgg        44
083000031008     C* Valorizzo i campi della e-m@ail
083100120305     C                   eval      wrkEml='cedalert@brt.it'
083200031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
083300031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
083400031008     C                             'SU AS/400'                         +':/N'+
083500031008     C                             ' '                                 +':/N'+
083600031008     C                             'RIFERIMENTI'                       +':/N'+
083700031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
083800031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
083900031008     C                             'Cliente:'+vltksc                   +':/N'+
084000031008     C                             'TipoFile:'+vlttip                  +':/N'+
084100031008     C                             ' '                                 +':/N'+
084200031008     C                             'MESSAGGIO'                         +':/N'+
084300031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
084400031008     C*
084500031008     C                   call(e)   'TIS701C'
084600031008     C                   parm                    wrkEml
084700031008     C                   parm                    wrkEmlOgg
084800031008     C                   parm                    wrkEmlMsg
084900031008     C*
085000031008     C                   endsr
085100990910
085200000706
085300000706
085400990908      /TITLE Operazioni iniziali.
085500990908     C     *inzsr        begsr
085600990908     C*
085700990908     C     *entry        plist
085800991026     C     wrkksc        parm                    prmksc
085900991026     C     wrkksu        parm      wrkksu        prmksu
086000991026     C     wrktip        parm                    prmtip
086100991026     C                   parm      wrkesito      prmesito
086200000623     C                   parm                    prmprog
086300000623     C                   parm                    prmsrv
086400000628     C                   parm                    prmope
086500000713     C                   parm                    prmnfp
086600070301     C                   parm                    wprmout          10
086700070301     C*
086800070301     C* Gestisco il ricevimento parametri
086900070301     C                   eval      prmout = *blanks
087000070301     C                   if        %parms = 9
087100070301     C                   eval      prmout = wprmout
087200070301     C                   endif
087300991104     C*
087400991105     C     k02vss02      klist
087500991105     C                   kfld                    vssksu
087600991104     C                   kfld                    vssisv
087700991026     C*
087800991026     C     k03vir02      klist
087900991026     C                   kfld                    virksc
088000991026     C                   kfld                    virtip
088100991026     C                   kfld                    virdti
088200991026     C*
088300991026     C     k02vir02      klist
088400991026     C                   kfld                    virksc
088500991026     C                   kfld                    virtip
088600000706     C*
088700000706     C     k04vir02      klist
088800000706     C                   kfld                    vreksc
088900000706     C                   kfld                    vretfl
089000000706     C                   kfld                    vredti
089100990920     C*
089200990920     C     k01vtf01      klist
089300990920     C                   kfld                    vtftip
089400000622     C*
089500000622     C     keytbe        klist
089600000622     C                   kfld                    tbecod
089700000622     C                   kfld                    tbeke1
089800000712     C*
089900000712     C     k01vlt        klist
090000000712     C                   kfld                    vltprg
090100000712     C                   kfld                    vltksc
090200031008     C*
090300031008     C     k08vlt        klist
090400031008     C                   kfld                    wrkfit
090500031008     C                   kfld                    wrkmbt
090600000706     C*
090700991104     C* Reperisco nome sistema.
090800991104     C                   eval      qusbprv = %size(qusec)
090900991104     C                   call      'QWCRNETA'
091000991104     C                   parm                    rneta01
091100991104     C                   parm                    rneta02
091200991104     C                   parm                    rneta03
091300991104     C                   parm                    rneta04
091400991104     C                   parm                    qusec
091500991104     C                   eval      nait = rneta01c
091600990908     C*
091700990908     C                   endsr
091800991026** ctdata mc
091900991026ABCDEFGHILMN
