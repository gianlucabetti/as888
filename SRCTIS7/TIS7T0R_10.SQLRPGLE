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
034400070504     C                   if        §VIR01TM = 'T'
034500070504     C                   eval      wrkmbt = dwlprg                              membro = progressivo
034600070504     C                   eval      %subst(wrkmbt:3:2) = wrktip                  membro = prog + tipf
034700070504     C                   else
034800030526     C                   eval      wrkmbt = 'M' + wrkksu                        membro = 'M' + KSC
034900070504     C                   endif
035000070207     C                   endif
035100030519     C                   endif
035200000706     C* Controllo abilitazione al servizio.
035300000706     C                   exsr      chkvss
035400000706     C                   endif
035500000706     C                   endif
035600000706     C*
035700000706     C                   endsr
035800000706
035900000711
036000000711
036100000711      /TITLE Lancio l'eventuale PGM "post-traduzione"
036200000711     C*      (il PGM si riferisce unicamente a quello del tipo file "master")
036300000711     C*
036400000711     C     exepgm        begsr
036500000711     C* Lancio il pgm in variabile con relativi parametri
036600000711     C* (tutto ciò solo se chiaramente c'è qualcosa da lanciare)
036700000711    C                   if        wrkpgi <> *blanks
036800000712     C* Chiamo il pgm post traduzione.
036900000712     C                   call(e)   'TIS7T9C1'
037000000712     C                   parm      wrkfid        prmfid
037100000712     C                   parm      wrkmbd        prmmbd
037200000712     C                   parm      wrkfis        prmfis
037300000712     C                   parm      wrkmbs        prmmbs
037400031106     C                   parm      wrkfit        prmfit
037500031106     C                   parm      wrkmbt        prmmbt
037600000712     C                   parm                    wrkpgi
037700000712     C                   parm                    wrkppi
037800000712     C                   parm      *blanks       prmesito
037900000712     C* Se la call è finita in errore
038000000712     C* o il lancio del PGM ha avuto degli errori.
038100000712     C                   if        %error
038200000712     C                             or prmesito <> '0'
038300000712     C     k01vlt        setll     tivlt07l
038400000712     C                   if        %found(tivlt07l)
038500000712     C     k01vlt        reade     tivlt07l
038600000712     C                   dow       not %eof
038700000712     C                   if        vltsqz = *zeros and
038800000712     C                             (vltsts = '0' or vltsts = '1')
038900000712     C                   eval      vltsts = '2'
039000000712     C                   eval      wrkmsg = 'Errori solo in post traduzione.'
039100000712     C                   eval      vltmsg =
039200000712     C                               %trim(sysname)
039300000712     C                             + '/'
039400000712     C                             + %trim(psjobname)
039500000712     C                             + '/'
039600000712     C                             + %trim(psuser)
039700000712     C                             + '/'
039800000712     C                             + %trim(%editc(psjobnbr:'Z'))
039900000712     C                             + %trim(wrkmsg)
040000000712     C                   update    tivlt700
040100000712     C                   endif
040200000712     C     k01vlt        reade     tivlt07l
040300000712     C                   enddo
040400000712     C                   endif
040500000712     C                   endif
040600000712     C*
040700000711     C                   endif
040800000711     C*
040900000711     C                   endsr
041000000711
041100000711
041200000626
041300000626      /TITLE Valorizzazione Progressivo Applicazione
041400000626     C     calprog       begsr
041500000626     C*
041600000706     C                   if        dwlprg = *all'0'
041700000627     C                   read(e)   tis7prgf
041800000627     C                   if        not %error
041900000627     C                   eval      dwlprg = f_tis7prgf
042000000627     C*
042100000626     C* Testo i parametri in ingresso per effettuare alcune considerazioni
042200000719     C                   if        prmsrv <> *blanks
042300000719     C                   eval      vssisv = prmsrv
042400000719     C                   endif
042500000626     C*
042600000719     C                   if        prmprog = *blanks
042700051011     C                   move(p)   dwl_prg       wrkprg
042800000626     C                   add       1             wrkprg
042900051011     C                   move(p)   wrkprg        dwl_prg
043000051011     C                   movel     vssisv        dwl_isv
043100051207     C*                  if        wrktip <> *blanks
043200051207     C*                  movel     wrktip        dwl_tip
043300051207     C*                  else
043400051207     C*                  movel     'XX'          dwl_tip
043500051207     C*                  endif
043600040123     C                   eval      f_tis7prgf = dwlprg
043700040123     C                   update    tis7prg0
043800000626     C                   else
043900040123     C                   unlock(e) tis7prgf
044000000626     C                   eval      dwlprg = prmprog
044100000626     C                   endif
044200000627     C*
044300000627     C                   endif
044400000706     C                   endif
044500000626     C*
044600000626     C                   endsr
044700991104
044800000706
044900000706
045000000706
045100991104      /TITLE Controllo abilitazione servizio.
045200991104     C     chkvss        begsr
045300991104     C*
045400000914     C                   eval      vssok = *off
045500000914     C*
045600991105     C                   eval      vssksu = wrkksu
045700000914     C     k02vss02      setll     tivss02l
045800000914     C     k02vss02      reade     tivss02l
045900000914     C                   dow       not %eof
046000000914     C                   if        vssdde <= wrkoggi and
046100000914     C                             vssdsc >= wrkoggi and
046200000914     C                             vssvat <> 'A'     and
046300000914     C                             vssvat <> 'S'
046400000914     C                   eval      vssok = *on
046500000914     C                   leave
046600000914     C                   endif
046700000914     C     k02vss02      reade     tivss02l
046800000914     C                   enddo
046900000914     C*
047000000914     C                   if        vssok = *off
047100991104     C*
047200000622     C                   exsr      inztivlt
047300000329     C                   eval      vltsts = '2'
047400991104     C                   eval      vltmsg =
047500991104     C                               %trim(sysname)
047600991104     C                             + '/'
047700991104     C                             + %trim(psjobname)
047800991104     C                             + '/'
047900991104     C                             + %trim(psuser)
048000991104     C                             + '/'
048100991104     C                             + %trim(%editc(psjobnbr:'Z'))
048200991104     C                             + ' Cliente non abilitato al servizio.'
048300000622     C                   exsr      wrttivlt
048400000622     C                   eval      wrkesito = 'D'
048500031008     C*
048600000622     C                   else
048700031008     C*
048800031008     C* Se fino a qui tutto ok, effettuo la verifica (importantissima) della univocità
048900031008     C* del progressivo rispetto a file/membro e cliente
049000031008     C                   movel     '0'           chkPRG            1
049100031008     C     k08vlt        setll     tivlt08l
049200031008     C                   if        %equal(tivlt08l)
049300031008     C     k08vlt        reade     tivlt08l
049400031008     C                   dow       not %eof(tivlt08l)
049500031008     C* Verifico se x medesimo file/membro il cliente è diverso da quello corrente (errore)
049600031008     C                   if        w_vltksc <> wrkksc
049700031008     C                   movel     '1'           chkPRG
049800031008     C                   endif
049900031008     C     k08vlt        reade     tivlt08l
050000031008     C                   enddo
050100031008     C                   endif
050200031008     C*
050300000706     C* Se veramente tutto è ok ==> Conversione/Traduzione.
050400031008     C                   if        chkPRG = '0'
050500000706     C                   exsr      traduci
050600031008     C                   else
050700031008     C*
050800031008     C                   exsr      inztivlt
050900031008     C                   eval      vltsts = '2'
051000031008     C                   eval      vltmsg =
051100031008     C                               %trim(sysname)
051200031008     C                             + '/'
051300031008     C                             + %trim(psjobname)
051400031008     C                             + '/'
051500031008     C                             + %trim(psuser)
051600031008     C                             + '/'
051700031008     C                             + %trim(%editc(psjobnbr:'Z'))
051800031008     C                             + ' Progressivo/membro NON univoco!!!'
051900031008     C                   exsr      wrttivlt
052000031008     C                   eval      wrkesito = 'F'
052100031008     C                   exsr      sndeml
052200031008     C                   exsr      uscita
052300031008     C                   endif
052400991104     C*
052500991104     C                   endif
052600991104     C*
052700991104     C                   endsr
052800000622
052900000622
053000000622      /TITLE Reperimento TIPO APPLICAZIONE da Tipo Servizio
053100000622     C     rtvtia        begsr
053200000622     C*
053300000622     C                   eval      tbecod = 'ISV'
053400000731     C                   eval      tbeke1 = virfi1
053500000622     C     keytbe        chain     tntbe01l
053600000622     C                   if        %found(tntbe01l)
053700020212     C                   movel     tbeuni        disv
053800000622     C                   endif
053900000622     C*
054000000622     C                   endsr
054100000706
054200000706
054300000706
054400000706      /TITLE Traduzione.
054500000706     C     traduci       begsr
054600031105     C*
054700031105     C* Reperisco informazioni membro da tradurre.
054800031105     C                   eval      mbrd04a = wrkfid
054900031105     C                   eval      mbrd05  = wrkmbd
055000031105     C                   exsr      rtvmbrd
055100031105     C* Controllo se il membro da tradurre ha dei record.
055200031105     C* oppure se è una operazione "multi-file" (trattasi di eventuali file facoltativi).
055300031105     C                   if        (qusei=*blanks and
055400060616     C                             qusnbrcr > 0) or prmnfp > '01'
055500060616     C                             or %trim(wrkfid) = '*'
055600030512     C* Verifico se il file/membro da tradurre nn sia allocato dalla procedura FTP/e-mail
055700030512     C                   call      'TIS7T9R2'
055800031008     C                   parm                    wrkfit
055900031008     C                   parm                    wrkmbt
056000030512     C                   parm      '0'           chkFTP            1
056100031105     C* SI record da tradurre e NO allocato da procedura FTP/e-mail => procedo con l'elaborazione
056200031105     C                   if        chkFTP = '0'
056300000706     C*
056400000706     C                   exsr      inztivlt
056500000706     C* Chiamo il pgm traduttore.
056600060616     C                   if        %trim(wrkfid) = '*' AND
056700060616     C                             %trim(wrkfis) = '*'
056800060616     C                   call(e)   'TIS7T9C3'
056900060616     C                   parm      wrkmbd        prmmbd
057000060616     C                   parm      wrkfit        prmfit
057100060616     C                   parm      wrkmbt        prmmbt
057200060616     C                   parm      wrkpgt        prmpgt
057300060616     C                   parm      wrkppt        prmppt
057400060616     C                   parm      *blanks       prmesito
057500060616     C                   else
057600000706     C                   call(e)   'TIS7T9C'
057700000706     C                   parm      wrkfid        prmfid
057800000706     C                   parm      wrkmbd        prmmbd
057900000706     C                   parm      wrkfis        prmfis
058000000706     C                   parm      wrkmbs        prmmbs
058100000706     C                   parm      wrkfit        prmfit
058200000706     C                   parm      wrkmbt        prmmbt
058300000706     C                   parm      wrkpgt        prmpgt
058400000706     C                   parm      wrkppt        prmppt
058500000706     C                   parm      *blanks       prmesito
058600060616     C                   endif
058700000706     C* Se la call è finita in errore
058800000706     C* o la traduzione ha avuto degli errori.
058900060320     C                   if        %error
059000060320     C                             or (prmesito <> '0' and
059100060320     C                                 prmesito <> 'Y')
059200000706     C                   eval      vltsts = '2'
059300000706     C                   eval      vltmsg =
059400000706     C                               %trim(sysname)
059500000706     C                             + '/'
059600000706     C                             + %trim(psjobname)
059700000706     C                             + '/'
059800000706     C                             + %trim(psuser)
059900000706     C                             + '/'
060000000706     C                             + %trim(%editc(psjobnbr:'Z'))
060100000706     C                             + ' Errori nella traduzione.'
060200000706     C                   else
060300060320     C                   if        prmesito = 'Y'
060400060320     C                   eval      wrkesito = prmesito
060500060320     C                   else
060600000706     C                   eval      wrkesito = '0'
060700060320     C                   endif
060800000706     C                   endif
060900000706     C*
061000000706     C                   exsr      wrttivlt
061100031105     C* SI record da tradurre ma allocato fa procedura FTP/e-mail
061200031105     C                   else
061300030512     C                   exsr      inztivlt
061400030512     C                   eval      vltsts = '2'
061500030512     C                   eval      vltmsg =
061600030512     C                               %trim(sysname)
061700030512     C                             + '/'
061800030512     C                             + %trim(psjobname)
061900030512     C                             + '/'
062000030512     C                             + %trim(psuser)
062100030512     C                             + '/'
062200030512     C                             + %trim(%editc(psjobnbr:'Z'))
062300030512     C                             + ' Cliente/TipoFile in uso da server FTP'
062400030512     C                   exsr      wrttivlt
062500030512     C                   eval      wrkesito = 'E'
062600030512     C                   endif
062700031105     C* NO record da tradurre
062800031105     C                   endif
062900000706     C*
063000000706     C                   endsr
063100000328
063200000706
063300000706
063400000328      /TITLE Inizializzo variabili "wrk".
063500000328     C     inzwrk        begsr
063600000328     C*
063700000328     C                   clear                   wrkfit
063800000328     C                   clear                   wrkmbt
063900000328     C                   clear                   wrkpgi
064000000328     C                   clear                   wrkppi
064100000328     C                   clear                   wrkfis
064200000328     C                   clear                   wrkmbs
064300000328     C*
064400000328     C                   endsr
064500991026
064600000706
064700000706
064800991026      /TITLE Inizializzo TIVLT00F.
064900991026     C     inztivlt      begsr
065000000804     C*
065100000804     C                   exsr      rtvtia
065200991026     C*
065300000328     C                   clear                   tivlt000
065400991026     C                   eval      vltksc = wrkksc
065500991026     C                   eval      vltksu = wrkksu
065600000721     C                   eval      vlttip = wrktip
065700991026     C                   eval      vltfld = wrkfit
065800991026     C                   eval      vltmbd = wrkmbt
065900991026     C                   eval      vltpgi = wrkpgi
066000991026     C                   eval      vltppi = wrkppi
066100020212     C                   if        §isvproc = 'F'
066200000804     C                   eval      vltsnd = 'D'
066300000804     C                   else
066400991026     C                   eval      vltsnd = vtfsnd
066500000804     C                   endif
066600991026     C                   eval      vltfls = wrkfis
066700991026     C                   eval      vltmbs = wrkmbs
066800991026     C                   eval      vltdat = wrkoggi
066900991115     C                   eval      vltora = wrkora
067000991026     C                   eval      vltsts = '0'
067100991027     C                   eval      vltmsg =
067200991104     C                               %trim(sysname)
067300991104     C                             + '/'
067400991104     C                             + %trim(psjobname)
067500991027     C                             + '/'
067600991027     C                             + %trim(psuser)
067700991027     C                             + '/'
067800991027     C                             + %trim(%editc(psjobnbr:'Z'))
067900000328     C                   z-add     *zeros        vltsqz
068000000731     C*
068100991026     C                   endsr
068200991026
068300000706
068400000731
068500000731      /TITLE Inizializzo parzialmente TIVLT00F.
068600000731     C     inztivltp     begsr
068700000731     C*
068800000731     C                   clear                   tivlt000
068900000731     C                   eval      vltksc = wrkksc
069000000731     C                   eval      vltksu = wrkksu
069100000731     C                   eval      vlttip = wrktip
069200000731     C                   eval      vltpgi = wrkpgi
069300000731     C                   eval      vltppi = wrkppi
069400000731     C                   eval      vltsnd = vtfsnd
069500000731     C                   eval      vltdat = wrkoggi
069600000731     C                   eval      vltora = wrkora
069700000731     C                   eval      vltsts = '0'
069800000731     C                   eval      vltmsg =
069900000731     C                               %trim(sysname)
070000000731     C                             + '/'
070100000731     C                             + %trim(psjobname)
070200000731     C                             + '/'
070300000731     C                             + %trim(psuser)
070400000731     C                             + '/'
070500000731     C                             + %trim(%editc(psjobnbr:'Z'))
070600000731     C                   z-add     *zeros        vltsqz
070700000731     C*
070800000731     C                   exsr      rtvtia
070900000731     C*
071000000731     C                   endsr
071100000731
071200000731
071300000706
071400991026      /TITLE Scrittura TIVLT00F.
071500000731     C     wrttivlt      begsr
071600991026     C*
071700000622     C* Queste assegnazioni le faccio qui xchè sono intimamante legate alla scrittura
071800000622     C* dell'archivio di log e condizionano fortemente l'intera procedura!!!
071900000731     C                   eval      vltisv = virfi1
072000020212     C                   eval      vlttia = §isvproc
072100000622     C                   eval      vltprg = dwlprg
072200000713     C                   eval      vltfl2 = prmnfp
072300991026     C                   write     tivlt000
072400040817     C*
072500050909 TESTC* Eseguo operazioni d scrittura record d log download x duplicazione d massa dati x nuova
072600040817     C* procedura scambio dati
072700040817     C* ...quindi eseguo prima d tutto l'override
072800050909  "  C*
072900050909     C*                  eval      COMMAND = 'OVRDBF FILE(TIVLTNEW)' +
073000050909     C*                            ' TOFILE(EDPFGVASMI/TIVLT00F)'
073100050909     C*                  eval      LUNG = %len(%trim(COMMAND))
073200050909  "  C*                  call(e)   'QCMDEXC'
073300050909     C*                  parm                    COMMAND
073400050909     C*                  parm                    LUNG             15 5
073500040817     C*
073600050909  "  C*                  if        not %error
073700050909     C*                  if        not %open(tivltnew)
073800050909     C*                  open(e)   tivltnew
073900050909     C*                  endif
074000050909  "  C*                  if        not %error
074100050909     C*                  write     tivltn00
074200050909     C*                  endif
074300050909     C*                  if        %open(tivltnew)
074400050909  "  C*                  close(e)  tivltnew
074500050909     C*                  endif
074600050909 TESTC*                  endif
074700991026     C*
074800991026     C                   endsr
074900991026
075000000706
075100000706
075200991026      /TITLE Operazioni finali.
075300991026     C     uscita        begsr
075400991026     C*
075500000627     C                   seton                                        lr
075600991026     C*
075700991026     C                   endsr
075800991026
075900000706
076000000706
076100991026      /TITLE Controllo formale parametri ricevuti.
076200991026     C     chkprm        begsr
076300991026     C*
076400991104     C                   if        wrkksu = *blanks
076500031008     C*
076600031008     C                   exsr      inztivlt
076700031008     C                   eval      vltsts = '2'
076800031008     C                   eval      vltmsg =
076900031008     C                               %trim(sysname)
077000031008     C                             + '/'
077100031008     C                             + %trim(psjobname)
077200031008     C                             + '/'
077300031008     C                             + %trim(psuser)
077400031008     C                             + '/'
077500031008     C                             + %trim(%editc(psjobnbr:'Z'))
077600031008     C                             + ' Parametro codice cliente non passato.'
077700031008     C                   exsr      wrttivlt
077800031008     C                   eval      wrkesito = 'A'
077900031008     C*
078000991026     C                   exsr      uscita
078100991026     C                   endif
078200991026     C*
078300991026     C                   if        wrktip = *blanks
078400031008     C*
078500031008     C                   exsr      inztivlt
078600031008     C                   eval      vltsts = '2'
078700031008     C                   eval      vltmsg =
078800031008     C                               %trim(sysname)
078900031008     C                             + '/'
079000031008     C                             + %trim(psjobname)
079100031008     C                             + '/'
079200031008     C                             + %trim(psuser)
079300031008     C                             + '/'
079400031008     C                             + %trim(%editc(psjobnbr:'Z'))
079500031008     C                             + ' Parametro tipo file non passato.'
079600031008     C                   exsr      wrttivlt
079700031008     C                   eval      wrkesito = 'B'
079800031008     C*
079900991026     C                   exsr      uscita
080000991026     C                   endif
080100991026     C*
080200991026     C                   clear                   wrkesito
080300991026     C*
080400991026     C                   endsr
080500991111
080600000706
080700000706
080800991111      /TITLE Reperisco informazioni membro.
080900991111     C     rtvmbrd       begsr
081000991111     C* Pulisco e imposto la DS degli errori.
081100000124     C                   clear                   qusec
081200991111     C                   eval      qusbprv = %size(qusec)
081300991111     C*
081400991111     C                   call      'QUSRMBRD'
081500991111     C     qusm0200      parm                    mbrd01
081600991111     C                   parm                    mbrd02
081700991111     C                   parm                    mbrd03
081800991111     C                   parm                    mbrd04
081900991111     C                   parm                    mbrd05
082000991111     C                   parm                    mbrd06
082100991111     C                   parm                    qusec
082200991111     C*
082300991111     C                   endsr
082400031008
082500031008
082600031008
082700031008
082800031008      /TITLE Compongo il testo e spedisco una e-m@ail
082900031008     C     sndeml        begsr
083000031008     C*
083100031008     C* Inizializzo variabili
083200031008     C                   movel     *blanks       wrkEml          253
083300031008     C                   movel     *blanks       wrkEmlMsg      5000
083400031008     C                   movel     *blanks       wrkEmlOgg        44
083500031008     C* Valorizzo i campi della e-m@ail
083600120305     C                   eval      wrkEml='cedalert@brt.it'
083700031008     C                   eval      wrkEmlOgg='VAS - WARNING!!!'
083800031008     C                   eval      wrkEmlMsg='VERIFICARE IL LOG DOWNLOAD '+
083900031008     C                             'SU AS/400'                         +':/N'+
084000031008     C                             ' '                                 +':/N'+
084100031008     C                             'RIFERIMENTI'                       +':/N'+
084200031008     C                             'Data:'+%editw(vltdat:'    /  /  ') +':/N'+
084300031008     C                             'Ora:'+%editw(vltora:'  :  :  ')    +':/N'+
084400031008     C                             'Cliente:'+vltksc                   +':/N'+
084500031008     C                             'TipoFile:'+vlttip                  +':/N'+
084600031008     C                             ' '                                 +':/N'+
084700031008     C                             'MESSAGGIO'                         +':/N'+
084800031008     C                             'ERRORE GRAVE: progressivo NON univoco!!!'
084900031008     C*
085000031008     C                   call(e)   'TIS701C'
085100031008     C                   parm                    wrkEml
085200031008     C                   parm                    wrkEmlOgg
085300031008     C                   parm                    wrkEmlMsg
085400031008     C*
085500031008     C                   endsr
085600990910
085700000706
085800000706
085900990908      /TITLE Operazioni iniziali.
086000990908     C     *inzsr        begsr
086100990908     C*
086200990908     C     *entry        plist
086300991026     C     wrkksc        parm                    prmksc
086400991026     C     wrkksu        parm      wrkksu        prmksu
086500991026     C     wrktip        parm                    prmtip
086600991026     C                   parm      wrkesito      prmesito
086700000623     C                   parm                    prmprog
086800000623     C                   parm                    prmsrv
086900000628     C                   parm                    prmope
087000000713     C                   parm                    prmnfp
087100070301     C                   parm                    wprmout          10
087200070301     C*
087300070301     C* Gestisco il ricevimento parametri
087400070301     C                   eval      prmout = *blanks
087500070301     C                   if        %parms = 9
087600070301     C                   eval      prmout = wprmout
087700070301     C                   endif
087800991104     C*
087900991105     C     k02vss02      klist
088000991105     C                   kfld                    vssksu
088100991104     C                   kfld                    vssisv
088200991026     C*
088300991026     C     k03vir02      klist
088400991026     C                   kfld                    virksc
088500991026     C                   kfld                    virtip
088600991026     C                   kfld                    virdti
088700991026     C*
088800991026     C     k02vir02      klist
088900991026     C                   kfld                    virksc
089000991026     C                   kfld                    virtip
089100000706     C*
089200000706     C     k04vir02      klist
089300000706     C                   kfld                    vreksc
089400000706     C                   kfld                    vretfl
089500000706     C                   kfld                    vredti
089600990920     C*
089700990920     C     k01vtf01      klist
089800990920     C                   kfld                    vtftip
089900000622     C*
090000000622     C     keytbe        klist
090100000622     C                   kfld                    tbecod
090200000622     C                   kfld                    tbeke1
090300000712     C*
090400000712     C     k01vlt        klist
090500000712     C                   kfld                    vltprg
090600000712     C                   kfld                    vltksc
090700031008     C*
090800031008     C     k08vlt        klist
090900031008     C                   kfld                    wrkfit
091000031008     C                   kfld                    wrkmbt
091100000706     C*
091200991104     C* Reperisco nome sistema.
091300991104     C                   eval      qusbprv = %size(qusec)
091400991104     C                   call      'QWCRNETA'
091500991104     C                   parm                    rneta01
091600991104     C                   parm                    rneta02
091700991104     C                   parm                    rneta03
091800991104     C                   parm                    rneta04
091900991104     C                   parm                    qusec
092000991104     C                   eval      nait = rneta01c
092100070504     C*
092200070504     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
092300070504     C
092400070504     C/EXEC SQL
092500070504     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
092600070504     C/END-EXEC
092700070504     C*
092800990908     C*
092900990908     C                   endsr
093000991026** ctdata mc
093100991026ABCDEFGHILMN
