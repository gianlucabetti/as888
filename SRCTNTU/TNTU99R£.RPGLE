000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200051214     H* TNTU99R *----------------------------------------------------*
000300051214     H* - LEGGE SPUNTE E FOGLI FIBRV / FNFVV
000400000000     H*--------------------------------------------------------------*
000500051214     ftntud01l  uf a e           k disk
000600051214     fFNFVV02L  if   e           k Disk
000700051215     ftabel00f  if   e           k Disk
000800051214     fFIBRV01L  if   e           k Disk
000900051215     d
001000051215     D npspck          S              2    DIM(90)
001100051215     d
001200051214     dkpjba          e ds
001300051214     dtntu05ds       e ds
001400060103     dtntudtads      e ds
001500051215     dDS7J           e ds
001600051215     d
001700051215     d Kspg            s                   Like(fvvspg)
001800051215     d Knpg            s                   Like(fvvnpg)
001900051215     d Kdata           s                   Like(fvvdfv)
002000051215     d Kfgs            s                   Like(fvvfgs)
002100051215     d oldfgs          s                   Like(fvvfgs)
002200051215     d Kmult           s                   Like(tudmult)
002300051215     d Knfv            s                   Like(brvnfv)
002400051215     d Kcli            s                   Like(tudcli) inz(0)
002500051215     d Kcod            s                   Like(tblcod)
002600051215     d codut           s              1  0 inz(1)
002700051215     d Dataiso         s               d   datfmt(*iso)
002800051215     d Datafine        s               d   datfmt(*iso)
002900030722     F**
003000900518     C***************************************************************
003100051214     c* Moltiplicatori calcolati:
003200051214     c*    150 - colli in IMA
003300051214     c*    180 - colli in IMG
003400051215     c*    170 - colli rietrati non consegnati
003500051215     c*    210 - spunte al varco no picking
003600051215     c*    190 - colli con anomalia
003700051214     c
003800051215     c                   if        t05mult<>*blanks
003900051215     c                   z-add     5             contamult         3 0
004000051215     c                   else
004100051215     c                   z-add     1             contamult         3 0
004200051215     c                   endif
004300051215     c* Lo cancello prima dovendo elaborare 2 categorie
004400051215     c                   if        t05mult='190' or t05mult=*blanks
004500051215     c                   eval      kmult='190'
004600051215     c* loop sulla data
004700051215     c     *iso          movel     t05dti        dataiso
004800051215     c     *iso          movel     t05dtf        datafine
004900051215    2c                   dow       dataiso<=datafine
005000051215     c                   exsr      CANCMULT
005100051215     c
005200051215     c                   adddur    1:*D          dataiso
005300051215     c                   enddo
005400051215     c                   endif
005500051214     c
005600051215    1c                   dow       contamult<=5
005700051215     c                   exsr      TipoMult
005800051215     c
005900051215     c* Imposta il giro sulla data
006000051215     c                   z-add     t05dti        kdata
006100051215    2c                   dow       kdata<=t05dtf
006200051215     c
006300051215     c                   if        kfgs=0
006400051215     c     kfvv          setll     fnfvv02l
006500051215     c     kfvv          reade     fnfvv02l
006600051215     c                   else
006700051215     c     kfvv2         setll     fnfvv02l
006800051215     c     kfvv2         reade     fnfvv02l
006900051215     c                   endif
007000051215     c
007100051215     c                   z-add     fvvfgs        oldfgs
007200051215     c                   clear                   contabrv
007300051215     c                   clear                   contacan
007400051215     c
007500051215    3c                   dow       not %eof(fnfvv02l)
007600051215     c
007700051215     c* Escludo gli annullati
007800051215     c                   if        fvvatb=' '
007900051215     c* Se impostato sottotipo foglio, verifico se uguale
008000051215     c                   if        (kspg<>*blanks and kspg=fvvspg) or
008100051215     c                              KSPG=*BLANKS
008200051215     c* a cambio p.o. scrivo tntud00f
008300051215     c                   if        fvvfgs<>oldfgs
008400051215     c                   exsr      SCriviTUD
008500051215     c
008600051215     c* Memorizzo anche la prima parte del contatore 190 lett col '150'
008700051215     c                   if        kmult='150'
008800051215     c                   z-add     contacan      contabrv
008900051215     c                   eval      kmult='190'
009000051215     c                   exsr      SCriviTUD
009100051215     c                   eval      kmult='150'
009200051215     c                   endif
009300051215     c
009400051215     c                   eval      oldfgs=fvvfgs
009500051215     c                   clear                   contabrv
009600051215     c                   clear                   contacan
009700051215     c                   endif
009800051215     c
009900051215     c                   exsr      LeggiBRV
010000051215     c                   endif
010100051215     c                   endif
010200051215     c
010300051215     c                   if        kfgs=0
010400051215     c     kfvv          reade     fnfvv02l
010500051215     c                   else
010600051215     c     kfvv2         reade     fnfvv02l
010700051215     c                   endif
010800051215    3c                   enddo
010900051215     c* Ultima scrittura
011000051215     c                   exsr      ScriviTUD
011100051215     c
011200051215     c* Memorizzo anche la prima parte del contatore 190  letto con '150'
011300051215     c                   if        kmult='150'
011400051215     c                   z-add     contacan      contabrv
011500051215     c                   eval      kmult='190'
011600051215     c                   exsr      SCriviTUD
011700051215     c                   eval      kmult='150'
011800051215     c                   endif
011900051215     c
012000051215     c* Aggiungo 1 giorno alla data
012100051215     c     *iso          movel     kdata         dataiso
012200051215     c                   adddur    1:*D          dataiso
012300051215     c                   movel     dataiso       kdata
012400051215    2c                   enddo
012500051215     c
012600051215     c                   add       1             contamult
012700051215     c                   enddo
012800060103
012900060103     c
013000060103     c* Se non richiesto un moltiplicatore o un p.o. aggiorno la tabella
013100060103     c*  TUP
013200060103     c                   if        t05mult=*blanks and t05fil=0
013300060103     c                   clear                   tntudtads
013400060103     c                   eval      §DTAazn = kcdaz
013500060103     c                   eval      §DTAdta = t05dtf
013600060103     c                   call      'TNTUDTAR'
013700060103     c                   parm                    tntudtads
013800060103     c                   endif
013900060103     c
014000060103     C*
014100941216     C*
014200051123     c
014300051123     C                   SETON                                        LR
014400991206     C**************************************************************************
014500051123     C     *INZSR        BEGSR
014600051214     c
014700051214     C     *ENTRY        PLIST
014800051214     C                   PARM                    kpjba
014900051214     c                   movel     kpjbu         tntu05ds
015000051123     c
015100051215     c     ktab          Klist
015200051215     c                   Kfld                    codut
015300051215     c                   Kfld                    kcod
015400051214     c     kfvv          Klist
015500051214     c                   Kfld                    knpg
015600051214     c                   Kfld                    kdata
015700051215     c     kfvv2         Klist
015800051214     c                   Kfld                    knpg
015900051214     c                   Kfld                    kdata
016000051214     c                   Kfld                    kfgs
016100051215     c     kbrv          Klist
016200051215     c                   Kfld                    fvvnpg
016300051215     c                   Kfld                    knfv
016400051215     c                   Kfld                    fvvfgs
016500051215     c     ktud          Klist
016600051215     c                   Kfld                    kmult
016700051215     c                   Kfld                    dataiso
016800051215     c                   Kfld                    oldfgs
016900051215     c                   Kfld                    kcli
017000051215     c     ktud2         Klist
017100051215     c                   Kfld                    kmult
017200051215     c                   Kfld                    dataiso
017300051215     c     ktud3         Klist
017400051215     c                   Kfld                    kmult
017500051215     c                   Kfld                    dataiso
017600051215     c                   Kfld                    kfgs
017700051123     c
017800051214     c                   if        t05fil>0
017900051214     c                   eval      kfgs=t05fil
018000051214     c                   endif
018100051214     c
018200051124    cc                   endsr
018300051214     C**************************************************************************
018400051214     c* Imposto i campi in base al tipo moltiplicatore
018500051214     C**************************************************************************
018600051215     C     TIPOMULT      BEGSR
018700051215     c
018800051215     c                   select
018900051215     c* Colli in IMA
019000051215     c                   when      contamult=1 or t05mult='150'
019100051214     c                   eval      knpg=3
019200051214     c                   eval      kspg='A'
019300051215     c                   eval      kmult='150'
019400051215     c* Colli in IMG
019500051215     c                   when      contamult=2 or t05mult='180'
019600051214     c                   eval      knpg=3
019700051214     c                   eval      kspg='G'
019800051215     c                   eval      kmult='180'
019900051215     c* Colli rientrati non consegnati
020000051215     c                   when      contamult=3 or t05mult='170'
020100051215     c                   eval      knpg=8
020200051215     c                   eval      kspg=' '
020300051215     c                   eval      kmult='170'
020400051215     c* spunte la varco no piking
020500051215     c                   when      contamult=4 or t05mult='210'
020600051215     c                   eval      knpg=4
020700051215     c                   eval      kspg=' '
020800051215     c                   eval      kmult='210'
020900051215     c
021000051215     c* Carico le pistole del picking per escluderle dal conteggio
021100051215     c                   eval      kcod='7J'
021200051215     c                   z-add     1             xx                3 0
021300051215     c     ktab          setll     tabel00f
021400051215     c     ktab          reade     tabel00f
021500051215     c
021600051215     c                   dow       not %eof(tabel00f)
021700051215     c                   if        tblflg=' '
021800051215     c                   movel     tbluni        ds7j
021900051215     c                   if        §7jpck='P'
022000051215     c                   movel     tblkey        npspck(xx)
022100051215     c                   add       1             xx
022200051215     c                   endif
022300051215     c                   endif
022400051215     c
022500051215     c     ktab          reade     tabel00f
022600051215     c                   enddo
022700051215     c
022800051215     c* spunte con anomalia
022900051215     c                   when      contamult=5 and t05mult=*blanks
023000051215     c                   eval      knpg=2
023100051215     c                   eval      kspg=' '
023200051215     c                   eval      kmult='190'
023300051215     c
023400051215     c* Se richiesto solo il 190 devo comunque fare 2 giri
023500051215     c                   when      contamult=5 and t05mult='190'
023600051215     c                   eval      knpg=3
023700051215     c                   eval      kspg='A'
023800051215     c                   eval      kmult='190'
023900051215     c                   eval      contamult=4
024000051215     c* clearo il campo t05mult per far leggere poi anche le spunte 2
024100051215     c                   clear                   t05mult
024200051215     c                   endsl
024300051214     c*
024400051214    cc                   endsr
024500051215     C**************************************************************************
024600051215     c* Leggo le spunte e conto
024700051215     C**************************************************************************
024800051215     C     LEGGIBRV      BEGSR
024900051215     c                   z-add     fvvnfv        knfv
025000051215     c
025100051215     C
025200051215     C     KBRV          SETLL     FIBRV01L
025300051215     C     KBRV          READE     FIBRV01L
025400051215     C                   DOW       not %eof(fibrv01l)
025500051215     c
025600051215     c* Per il moltiplicarore '210' escludo le pistole del picking
025700051215     c                   setoff                                       30
025800051215     c                   if        kmult='210'
025900051215     c                   movel     brvnps        w002a             2
026000051215     c     w002a         lookup    npspck                                 30
026100051215     c                   endif
026200051215     c
026300051215     c* Per il moltiplicarore '190' solo con anomalia
026400051215     c                   if        kmult='190'  and brvcan=' '
026500051215     c                   seton                                            30
026600051215     c                   endif
026700051215     c
026800051215     c                   if        not *in30
026900051215     c                   add       1             contabrv          9 0
027000051215     c                   endif
027100051215     c
027200051215     c                   if        kmult='150' and brvcan<>' '
027300051215     c                   add       1             contacan          9 0
027400051215     c                   endif
027500051215     c
027600051215     C     KBRV          READE     FIBRV01L
027700051215     c                   enddo
027800051215     c
027900051215     c                   ENDSR
028000051215     C**************************************************************************
028100051215     c* Scrivo il file TNTUD00F
028200051215     C**************************************************************************
028300051215     C     SCriviTUD     BEGSR
028400051215     c     *iso          movel     kdata         dataiso
028500051215     c
028600051215     c                   if        contabrv>0
028700051215     c
028800051215     c     ktud          chain     tntud01l
028900051215     c
029000051215     c* Per il motiplicatore '190' devo andare in add
029100051215     c*  perchè una parte dei colli l'ho trovata col '150'
029200051215     c                   if        kmult<>'190' or not %found(tntud01l)
029300051215     c                   z-add     contabrv      tudqta
029400051215     c                   else
029500051215     c                   add       contabrv      tudqta
029600051215     c                   endif
029700051215     c
029800051215     c
029900051215     c                   if        not %found(tntud01l)
030000051215     c                   eval      tuddta=dataiso
030100051215     c                   eval      tudmult=kmult
030200051215     c                   eval      tudfil=oldfgs
030300051215     c                   clear                   tudcli
030400051215     c                   write     tntud000
030500051215     c                   else
030600051215     c                   update    tntud000
030700051215     c                   endif
030800051215     c                   endif
030900051215     c
031000051215     c                   ENDSR
031100051215     C**************************************************************************
031200051215     c* Cancella il moltiplicatore se rielaborazione
031300051215     C**************************************************************************
031400051215     C     CANCMULT      BEGSR
031500051215     c                   if        kfgs>0
031600051215     c     ktud3         setll     tntud01l
031700051215     c                   else
031800051215     c     ktud2         setll     tntud01l
031900051215     c                   endif
032000051215     c
032100051215     c                   setoff                                       31
032200051215     c                   dow       not *in31
032300051215     c                   if        kfgs>0
032400051215     c     ktud3         delete    tntud01l                           31
032500051215     c                   else
032600051215     c     ktud2         delete    tntud01l                           31
032700051215     c                   endif
032800051215     c                   enddo
032900051215     c
033000051215     c                   ENDSR
