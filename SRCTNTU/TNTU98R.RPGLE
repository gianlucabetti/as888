000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200051219     H* TNTU98R *----------------------------------------------------*
000300051219     H* - LEGGE fogli viaggio partenza - FNFGV
000400051220     H* - LEGGE fogli viaggio partenza in arrivo - FNfVA
000500000000     H*--------------------------------------------------------------*
000600051214     ftntud01l  uf a e           k disk
000700051219     fFNFgV02L  if   e           k Disk
000800051220     fFNfva05L  if   e           k Disk
000900060324     fazcae01l  if   e           k Disk
001000060324     fazorg01l  if   e           k Disk
001100051215     d
001200051214     dkpjba          e ds
001300051214     dtntu05ds       e ds
001400060103     dtntudtads      e ds
001500060324     dog143          e ds
001600051215     d
001700060324     d ContaPO         s                   Like(tudqta)
001800060324     d Kepa            s                   Like(caeepa) inz('A')
001900051219     d Kdata           s                   Like(fgvdfv)
002000051219     d Kfgs            s                   Like(fgvlnp)
002100051215     d Kmult           s                   Like(tudmult)
002200051215     d Kcli            s                   Like(tudcli) inz(0)
002300051215     d codut           s              1  0 inz(1)
002400051215     d Dataiso         s               d   datfmt(*iso)
002500051220     d
002600051220     Ifnfva000
002700051220     I              fvaatb                      fgvatb
002800051220     I              fvalai                      fgvlnp
002900051220     I              fvadfa                      fgvdfv
003000030722     F**
003100900518     C***************************************************************
003200051214     c* Moltiplicatori calcolati:
003300051219     c*    010 - fogli viaggio partenza
003400051220     c*    020 - fogli viaggio partenza in arrivo
003500060317     c*    022 - fogli viaggio partenza in arrivo senza transiti
003600060324     c*    600 - p.o. validi in base alla data decorrenza da azcae da FGV
003700051214     c
003800051215     c                   if        t05mult<>*blanks
003900051220     c                   z-add     2             contamult         3 0
004000051215     c                   else
004100051215     c                   z-add     1             contamult         3 0
004200051215     c                   endif
004300051214     c
004400051220    1c                   dow       contamult<=2
004500051215     c                   exsr      TipoMult
004600051220     c
004700051220     c                   if        t05fil=0
004800051220     c                   clear                   kfgs
004900051220     c                   endif
005000051219     c
005100060102     c* Cerco i p.o. perchè la lf è per linea /data e non viceversa
005200051219     c
005300051219     c                   if        t05fil=0
005400051220     c
005500051220     c                   if        kmult='010'
005600051219     c     kfgs          setgt     fnfgv02l
005700051219     c                   read      fnfgv02l
005800051220     c                   else
005900051220     c     kfgs          setgt     fnfva05l
006000051220     c                   read      fnfva05l
006100051220     c                   endif
006200051220     c
006300051219     c                   else
006400051220     c                   if        kmult='010'
006500051219     c     kfgs          setll     fnfgv02l
006600051219     c     kfgs          reade     fnfgv02l
006700051220     c                   else
006800051220     c     kfgs          setll     fnfva05l
006900051220     c     kfgs          reade     fnfva05l
007000051220     c                   endif
007100051220     c
007200051219     c                   endif
007300051219     c
007400051220    2c                   dow       not %eof
007500051219     c                   z-add     fgvlnp        kfgs
007600051219     c                   clear                   contafgv          9 0
007700060317     c                   clear                   contafgvnotran    9 0
007800060324     c                   clear                   welabpo           1
007900051215     c
008000051215     c* Imposta il giro sulla data
008100051215     c                   z-add     t05dti        kdata
008200051219    3c                   dow       kdata<=t05dtf
008300051215     c
008400051220     c                   if        kmult='010'
008500051219     c     kfgv          setll     fnfgv02l
008600051219     c     kfgv          reade     fnfgv02l
008700051220     c                   else
008800051220     c     kfgv          setll     fnfva05l
008900051220     c     kfgv          reade     fnfva05l
009000051220     c                   endif
009100051220
009200051220    4c                   dow       not %eof
009300051215     c
009400051215     c* Escludo gli annullati
009500051219    5c                   if        fgvatb=' '
009600051219     c                   add       1             contafgv
009700060324     c* Verifico se esiste almeno un foglio abbinato a traino, per poi
009800060324     c*  contare i p.o. per le buste
009900060324     c                   if        kmult='010'
010000060324     c                   if        fgvtrn>0 and fgvttr<>'D' and fgvttr<>'H'
010100060324     c                             and fgvttr<>'I' and fgvttr<>'L'
010200060324     c                   eval      welabpo='S'
010300060324     c                   endif
010400060324     c                   endif
010500060317     c
010600060317     c* fogli senza transiti solo in arrivo
010700060317     c                   if        kmult<>'010' and fvaatr=' '
010800060317     c                   add       1             contafgvnotran
010900060317    5c                   endif
011000060317    5c                   endif
011100060317     c
011200051219
011300051220     c                   if        kmult='010'
011400051219     c     kfgv          reade     fnfgv02l
011500051220     c                   else
011600051220     c     kfgv          reade     fnfva05l
011700051220     c                   endif
011800051220     c
011900051219    4c                   enddo
012000051219     c
012100051215     c                   exsr      SCriviTUD
012200060324     c
012300060324     c* elaboro il conteggio dei p.o.
012400060324     c                   if        welabpo='S'
012500060324     c                   EXSR      ElaboraPO
012600060324     c                   clear                   welabpo           1
012700060324     c                   endif
012800060317     c
012900060317     c* Per moltiplicatore  020 scrivo anche lo 022
013000060317     c                   if        kmult<>'010'
013100060317     c                   eval      kmult='022'
013200060317     c                   eval      contafgv=contafgvnotran
013300060317     c                   exsr      SCriviTUD
013400060317     c                   eval      kmult='020'
013500060317     c                   endif
013600051215     c
013700051219     c                   clear                   contafgv
013800060317     c                   clear                   contafgvnotran
013900051219     c
014000051219     c* Aggiungo 1 giorno alla data
014100051219     c     *iso          movel     kdata         dataiso
014200051219     c                   adddur    1:*D          dataiso
014300051219     c                   movel     dataiso       kdata
014400051219    3c                   enddo
014500051215     c
014600051219     c                   if        t05fil=0
014700051220     c
014800051220     c                   if        kmult='010'
014900051219     c     kfgs          setgt     fnfgv02l
015000051219     c                   read      fnfgv02l
015100051220     c                   else
015200051220     c     kfgs          setgt     fnfva05l
015300051220     c                   read      fnfva05l
015400051220     c                   endif
015500051220     c
015600051219     c                   else
015700051219     c* Se impostato un p.o. finito la lettura
015800051219     c                   leave
015900051219     c                   endif
016000051219     c
016100051219    2c                   enddo
016200051215     c
016300051215     c                   add       1             contamult
016400051215     c                   enddo
016500060103     c
016600060103     c* Se non richiesto un moltiplicatore o un p.o., aggiorno la tabella
016700060103     c*  TUP
016800060103     c                   if        t05mult=*blanks and t05fil=0
016900060103     c                   clear                   tntudtads
017000060103     c                   eval      §DTAazn = kcoaz
017100060103     c                   eval      §DTAdta = t05dtf
017200060103     c                   call      'TNTUDTAR'
017300060103     c                   parm                    tntudtads
017400060103     c                   endif
017500060103     c
017600941216     C*
017700051123     c
017800051123     C                   SETON                                        LR
017900991206     C**************************************************************************
018000051123     C     *INZSR        BEGSR
018100051214     c
018200051214     C     *ENTRY        PLIST
018300051214     C                   PARM                    kpjba
018400051214     c                   movel     kpjbu         tntu05ds
018500051123     c
018600051219     c     kfgv          Klist
018700051219     c                   Kfld                    kfgs
018800051214     c                   Kfld                    kdata
018900051215     c     ktud          Klist
019000051215     c                   Kfld                    kmult
019100051215     c                   Kfld                    dataiso
019200051219     c                   Kfld                    kfgs
019300051215     c                   Kfld                    kcli
019400051214     c
019500051219     c                   if        t05fil>0
019600051219     c                   eval      kfgs=t05fil
019700051219     c                   endif
019800051219     c
019900051124    cc                   endsr
020000051214     C**************************************************************************
020100051214     c* Imposto i campi in base al tipo moltiplicatore
020200051214     C**************************************************************************
020300051215     C     TIPOMULT      BEGSR
020400051215     c
020500051215     c                   select
020600051220     c* Numero fogli viaggio partenza
020700051219     c                   when      contamult=1 or t05mult='010'
020800051219     c                   eval      kmult='010'
020900060324     c                   when      contamult=1 or t05mult='600'
021000060324     c                   eval      kmult='010'
021100051220     c* Numero fogli viaggio partenza abbinati
021200051220     c                   when      contamult=2 or t05mult='020'
021300051220     c                   eval      kmult='020'
021400060317     c                   when      contamult=2 or t05mult='022'
021500060317     c                   eval      kmult='020'
021600051215     c                   endsl
021700051219     c                   ENDSR
021800051215     C**************************************************************************
021900051215     c* Scrivo il file TNTUD00F
022000051215     C**************************************************************************
022100051215     C     SCriviTUD     BEGSR
022200051215     c     *iso          movel     kdata         dataiso
022300051215     c
022400051219     c                   if        contafgv>0
022500051215     c
022600051215     c     ktud          chain     tntud01l
022700051215     c
022800051219     c                   z-add     contafgv      tudqta
022900051215     c
023000051215     c                   if        not %found(tntud01l)
023100051215     c                   eval      tuddta=dataiso
023200051215     c                   eval      tudmult=kmult
023300051219     c                   eval      tudfil=kfgs
023400051215     c                   clear                   tudcli
023500051215     c                   write     tntud000
023600051215     c                   else
023700051215     c                   update    tntud000
023800051215     c                   endif
023900051215     c                   endif
024000051215     c
024100051215     c                   ENDSR
024200060324     C**************************************************************************
024300060324     c* Elabora i p.o. per i quali aperta una busta
024400060324     C**************************************************************************
024500060324     C     ElaboraPO     BEGSR
024600060324     c                   clear                   contapo
024700060324     c
024800060324     c* Leggo da azcae tutti i p.o. linee di arrivo escluso estero
024900060324     c     kepa          setll     aZCAE01L
025000060324     c     kepa          reade     AZCAE01L
025100060324
025200060324    4c                   dow       not %eof
025300060324     c
025400060324     c* Escludo gli annullati
025500060324    5c                   if        CAEATB=' ' and caetfp=0
025600060324     C                   if        kdata>=caedde and kdata<=caedsc
025700060324     c* Escluso estero
025800060324     c     caetfa        chain     azorg01l
025900060324     c                   if        %found(azorg01l)
026000060324     c                   movel     orgde3        og143
026100060324     c                   if        §ogntw<>'FED 'and §ogntw<>'DPD' and
026200060324     c                             §ogntw<>'EEX' and §ogntw<>'EUP'
026300060324     c                   add       1             contaPO
026400060324     c                   endif
026500060324     c                   endif
026600060324     c                   endif
026700060324     c                   endif
026800060324     c
026900060324     c     kepa          reade     AZCAE01L
027000060324     c                   enddo
027100060324     c
027200060324     c                   eval      kmult='600'
027300060324     c                   eval      contafgv=contaPO
027400060324     c                   exsr      SCriviTUD
027500060324     c* Reimposto moltiplicatore 101
027600060324     c                   eval      kmult='010'
027700060324     c                   ENDSR
