000100020920      *--------------------------------------------------------------------------------------------*
000200020920      *?NB: INVIARE IL PGM ANCHE SU AS777 IN STRATEOBJ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
000300020920      *--------------------------------------------------------------------------------------------*
000400991018     H*------------------------------------------------------------------------*
000500991018     H* Reperimento codici clienti unificanti -
000600991018     H*------------------------------------------------------------------------*
000700971103     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000800991026     f*---------------------------------------------------------------------
000900991026      *                      Indicazioni d'uso
001000991026      *  x eventuali prove e vedere i risultati della ricerca esiste il PGM
001100991109      *  TIBS11R RICHIAMABILE TRAMITE KPLANCIO oppure menu prova silvio
001200991026      *
001300991026      *Campi di INPUT
001400991026      *  codice      11,0 è lungo undici perchè esistono nell'archivio anche
001500991026      *                   i codici dei potenziali
001600991026      *  tipo legame  2 A è il tipo legame deve essere presente nella tab TLE
001700991026      *
001800991026      *  data         8 0 viene assunta udate se non impostata
001900991026      *  tipo ricerca 1 A effettua la ricerca per padre o figlio valori(P/F)
002000991026      *
002100991026      *         OUTPUT
002200991026      *  codice pad. 11,0 viene restituito sempre
002300991026      *  campone   5500   contenente 500 codici figli
002400991026      *
002500991026      *  non vengono considerati i codici sospesi o annullati
002600991026      *--------------------------------------------------------------------
002700991026     F* DATA BASE
002800971110     F
002900991018     Ftikun01l  IF   E           K DISK
003000991018     Ftikun02l  IF   E           K DISK    rename(tikun000:tikun2)
003100991018     d tibs10        E DS                  EXTNAME(tibs10ds)                    *ricerca capiconto
003200020328     d  skc                   21   5520  0
003300991018     d                                     DIM(500)
003400020405     D TipExe          S              1
003500991018     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
003600991018     c**********************************************************************
003700991018     c     *entry        plist
003800991018     c                   parm                    tibs10
003900020405     c                   parm                    ParTipExe         1
004000020405     C*
004100020405     C* Testo il numero di parametri ricevuti per evitare errori in chiamata da programmi
004200020405     C* precedenti che usano questo programma passando unicamente la DS di procedura
004300020405     C                   if        %parms = 1
004400020405     C                   eval      TipExe = *blanks
004500020405     C                   else
004600020405     C                   eval      TipExe = ParTipExe
004700020405     C                   endif
004800020405     C*
004900991026     c                   if        d10drf = 0
005000991026     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
005100991026     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
005200020405     C                   MOVEL     WN8           WN8_A             8            *DATA (8) IN G/M/AA
005300020405     C                   EVAL      WN8_A=%subst(WN8_A:5:4)+%subst(WN8_A:3:2)+
005400020405     C                                   %subst(WN8_A:1:2)
005500020405     C                   MOVE(P)   WN8_A         d10drf
005600991026     c                   endif
005700020405     C*
005800991018     C     KEYkun        KLIST
005900040402     C                   KFLD                    d10tle                         *TIPO LEGAME
006000040402     C                   KFLD                    KCEaz                          *COD. AZIENDA PADRE
006100040402     C                   KFLD                    KCEcod           11 0          *COD. CLIENTE PADRE
006200040402     C*
006300040402     C     KEYkunF       KLIST
006400040402     C                   KFLD                    d10tle                         *TIPO LEGAME
006500040402     C                   KFLD                    KCEaz                          *COD. AZIENDA FIGLIO
006600040402     C                   KFLD                    KCEcof           11 0          *COD. CLIENTE FIGLIO
006700020405     C*
006800991018     c                   move      *blank        kceaz             3
006900991018     c                   move      *zeros        skc
007000020405     C*
007100020405     C                   if        TipExe <> 'C'
007200991018     c                   exsr      main
007300020405     C                   endif
007400991018     c*
007500020405     C                   if        TipExe = *blanks OR
007600020405     C                             TipExe = 'C'
007700971110     c                   seton                                        lr
007800020405     C                   else
007900020405     C                   seton                                        rt
008000020405     C                   endif
008100971104     c**
008200991018     c*---------------------------------------------------
008300991018     c     main          begsr
008400991018     c*---------------------------------------------------
008500991018     c                   z-add     d10cod        kcecod
008600040402     c                   z-add     d10cod        kcecof
008700991109     c                   if        d10paf = 'F'
008800991018     c                   exsr      padre
008900991018     c                   else
009000040402     c*
009100040402     c* Ciclo su tutti i legami in cui il codice cercato risulta come figlio
009200040402     c     keykunF       setll     tikun02l                               99
009300040402     c                   if        %equal(tikun02l)
009400040402     c     keykunF       reade     tikun02l
009500040402     c                   dow       not %eof(tikun02l)
009600040402     c* Testo se la data d riferimento rientra nel periodo decorrenza/scadenza del legame
009700040402     c                   if        d10drf >= kundde and
009800040402     c                             d10drf <= kundsc and
009900040402     c                             kunvat = *blank
010000040402     c                   z-add     kuncop        kcecod
010100040402     c                   exsr      padre
010200040402     c                   endif
010300040402     c     keykunF       reade     tikun02l
010400040402     c                   enddo
010500040402     c                   endif
010600040402     c                   endif
010700040402     c*
010800001121     c                   if        skc(1) = 0 OR
010900001121     c                             d10cop = 0
011000991018     c                   move      '1'           d10err
011100991018     c                   move      msg(1)        d10msg
011200991018     c                   z-add     0             d10cop
011300001121     c                   move      *zeros        skc
011400991018     c                   else
011500991018     c                   move      *blank        d10err
011600991018     c                   move      *blank        d10msg
011700040402     c                   endif
011800991018     c                   endsr
011900991018     c*---------------------------------------------------
012000991018     c     padre         begsr
012100991018     c*---------------------------------------------------
012200991018     c                   z-add     0             i
012300001121     c                   z-add     kcecod        d10cop
012400991018     c     keykun        setll     tikun01l
012500991018     c                   do        *hival
012600991018     c     keykun        reade     tikun01l                               99
012700991018     c   99              leave
012800991109      *imposto il padre come primo elemento della ds /skiera dei figli
012900991018     c                   if        d10drf >= kundde and
013000991026     c                             d10drf <= kundsc and
013100991026     c                             kunvat = *blank
013200991109     c                   if        i = 0
013300991109     c                   add       1             i
013400991109     c                   move      kuncop        skc(i)
013500991109     c                   end
013600991018     c                   add       1             i                 3 0
013700991018     c                   move      kuncof        skc(i)
013800991018     c                   endif
013900991018     c                   enddo
014000001121      *
014100001121      * se sta cercando il padre, lo valorizza in OUTPUT solo se il codice INPUT è valido
014200001121if  1c                   IF        d10paf = 'P'                                 *cerca il padre
014300001121     c     d10cod        LOOKUP    skc                                    98
014400001121if  2c                   IF        NOT *in98                                    *figlio non caricato
014500001121     c                   CLEAR                   d10cop
014600001121e   2c                   ENDIF
014700001121e   1c                   ENDIF
014800001121     c*
014900991018     c                   endsr
015000991018** MSG - MESSAGGI
015100991018TIBS10R- Nessun codice estratto con i parametri immessi                        1
015200991018TIBS10R- 2                                                                     2
