000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200051219     H* TNTU98R *----------------------------------------------------*
000300051219     H* - LEGGE fogli viaggio partenza - FNFGV
000400051220     H* - LEGGE fogli viaggio partenza in arrivo - FNfVA
000500000000     H*--------------------------------------------------------------*
000600051214     ftntud01l  uf a e           k disk
000700051219     fFNFgV02L  if   e           k Disk
000800051220     fFNfva05L  if   e           k Disk
000900051215     d
001000051214     dkpjba          e ds
001100051214     dtntu05ds       e ds
001200060103     dtntudtads      e ds
001300051215     d
001400051219     d Kdata           s                   Like(fgvdfv)
001500051219     d Kfgs            s                   Like(fgvlnp)
001600051215     d Kmult           s                   Like(tudmult)
001700051215     d Kcli            s                   Like(tudcli) inz(0)
001800051215     d codut           s              1  0 inz(1)
001900051215     d Dataiso         s               d   datfmt(*iso)
002000051220     d
002100051220     Ifnfva000
002200051220     I              fvaatb                      fgvatb
002300051220     I              fvalai                      fgvlnp
002400051220     I              fvadfa                      fgvdfv
002500030722     F**
002600900518     C***************************************************************
002700051214     c* Moltiplicatori calcolati:
002800051219     c*    010 - fogli viaggio partenza
002900051220     c*    020 - fogli viaggio partenza in arrivo
003000060317     c*    022 - fogli viaggio partenza in arrivo senza transiti
003100051214     c
003200051215     c                   if        t05mult<>*blanks
003300051220     c                   z-add     2             contamult         3 0
003400051215     c                   else
003500051215     c                   z-add     1             contamult         3 0
003600051215     c                   endif
003700051214     c
003800051220    1c                   dow       contamult<=2
003900051215     c                   exsr      TipoMult
004000051220     c
004100051220     c                   if        t05fil=0
004200051220     c                   clear                   kfgs
004300051220     c                   endif
004400051219     c
004500060102     c* Cerco i p.o. perchè la lf è per linea /data e non viceversa
004600051219     c
004700051219     c                   if        t05fil=0
004800051220     c
004900051220     c                   if        kmult='010'
005000051219     c     kfgs          setgt     fnfgv02l
005100051219     c                   read      fnfgv02l
005200051220     c                   else
005300051220     c     kfgs          setgt     fnfva05l
005400051220     c                   read      fnfva05l
005500051220     c                   endif
005600051220     c
005700051219     c                   else
005800051220     c                   if        kmult='010'
005900051219     c     kfgs          setll     fnfgv02l
006000051219     c     kfgs          reade     fnfgv02l
006100051220     c                   else
006200051220     c     kfgs          setll     fnfva05l
006300051220     c     kfgs          reade     fnfva05l
006400051220     c                   endif
006500051220     c
006600051219     c                   endif
006700051219     c
006800051220    2c                   dow       not %eof
006900051219     c                   z-add     fgvlnp        kfgs
007000051219     c                   clear                   contafgv          9 0
007100060317     c                   clear                   contafgvnotran    9 0
007200051215     c
007300051215     c* Imposta il giro sulla data
007400051215     c                   z-add     t05dti        kdata
007500051219    3c                   dow       kdata<=t05dtf
007600051215     c
007700051220     c                   if        kmult='010'
007800051219     c     kfgv          setll     fnfgv02l
007900051219     c     kfgv          reade     fnfgv02l
008000051220     c                   else
008100051220     c     kfgv          setll     fnfva05l
008200051220     c     kfgv          reade     fnfva05l
008300051220     c                   endif
008400051220
008500051220    4c                   dow       not %eof
008600051215     c
008700051215     c* Escludo gli annullati
008800051219    5c                   if        fgvatb=' '
008900051219     c                   add       1             contafgv
009000060317     c
009100060317     c* fogli senza transiti solo in arrivo
009200060317     c                   if        kmult<>'010' and fvaatr=' '
009300060317     c                   add       1             contafgvnotran
009400060317    5c                   endif
009500060317    5c                   endif
009600060317     c
009700051219
009800051220     c                   if        kmult='010'
009900051219     c     kfgv          reade     fnfgv02l
010000051220     c                   else
010100051220     c     kfgv          reade     fnfva05l
010200051220     c                   endif
010300051220     c
010400051219    4c                   enddo
010500051219     c
010600051215     c                   exsr      SCriviTUD
010700060317     c
010800060317     c* Per moltiplicatore  020 scrivo anche lo 022
010900060317     c                   if        kmult<>'010'
011000060317     c                   eval      kmult='022'
011100060317     c                   eval      contafgv=contafgvnotran
011200060317     c                   exsr      SCriviTUD
011300060317     c                   eval      kmult='020'
011400060317     c                   endif
011500051215     c
011600051219     c                   clear                   contafgv
011700060317     c                   clear                   contafgvnotran
011800051219     c
011900051219     c* Aggiungo 1 giorno alla data
012000051219     c     *iso          movel     kdata         dataiso
012100051219     c                   adddur    1:*D          dataiso
012200051219     c                   movel     dataiso       kdata
012300051219    3c                   enddo
012400051215     c
012500051219     c                   if        t05fil=0
012600051220     c
012700051220     c                   if        kmult='010'
012800051219     c     kfgs          setgt     fnfgv02l
012900051219     c                   read      fnfgv02l
013000051220     c                   else
013100051220     c     kfgs          setgt     fnfva05l
013200051220     c                   read      fnfva05l
013300051220     c                   endif
013400051220     c
013500051219     c                   else
013600051219     c* Se impostato un p.o. finito la lettura
013700051219     c                   leave
013800051219     c                   endif
013900051219     c
014000051219    2c                   enddo
014100051215     c
014200051215     c                   add       1             contamult
014300051215     c                   enddo
014400060103     c
014500060103     c* Se non richiesto un moltiplicatore o un p.o., aggiorno la tabella
014600060103     c*  TUP
014700060103     c                   if        t05mult=*blanks and t05fil=0
014800060103     c                   clear                   tntudtads
014900060103     c                   eval      §DTAazn = kcoaz
015000060103     c                   eval      §DTAdta = t05dtf
015100060103     c                   call      'TNTUDTAR'
015200060103     c                   parm                    tntudtads
015300060103     c                   endif
015400060103     c
015500941216     C*
015600051123     c
015700051123     C                   SETON                                        LR
015800991206     C**************************************************************************
015900051123     C     *INZSR        BEGSR
016000051214     c
016100051214     C     *ENTRY        PLIST
016200051214     C                   PARM                    kpjba
016300051214     c                   movel     kpjbu         tntu05ds
016400051123     c
016500051219     c     kfgv          Klist
016600051219     c                   Kfld                    kfgs
016700051214     c                   Kfld                    kdata
016800051215     c     ktud          Klist
016900051215     c                   Kfld                    kmult
017000051215     c                   Kfld                    dataiso
017100051219     c                   Kfld                    kfgs
017200051215     c                   Kfld                    kcli
017300051214     c
017400051219     c                   if        t05fil>0
017500051219     c                   eval      kfgs=t05fil
017600051219     c                   endif
017700051219     c
017800051124    cc                   endsr
017900051214     C**************************************************************************
018000051214     c* Imposto i campi in base al tipo moltiplicatore
018100051214     C**************************************************************************
018200051215     C     TIPOMULT      BEGSR
018300051215     c
018400051215     c                   select
018500051220     c* Numero fogli viaggio partenza
018600051219     c                   when      contamult=1 or t05mult='010'
018700051219     c                   eval      kmult='010'
018800051220     c* Numero fogli viaggio partenza abbinati
018900051220     c                   when      contamult=2 or t05mult='020'
019000051220     c                   eval      kmult='020'
019100060317     c                   when      contamult=2 or t05mult='022'
019200060317     c                   eval      kmult='020'
019300051215     c                   endsl
019400051219     c                   ENDSR
019500051215     C**************************************************************************
019600051215     c* Scrivo il file TNTUD00F
019700051215     C**************************************************************************
019800051215     C     SCriviTUD     BEGSR
019900051215     c     *iso          movel     kdata         dataiso
020000051215     c
020100051219     c                   if        contafgv>0
020200051215     c
020300051215     c     ktud          chain     tntud01l
020400051215     c
020500051219     c                   z-add     contafgv      tudqta
020600051215     c
020700051215     c                   if        not %found(tntud01l)
020800051215     c                   eval      tuddta=dataiso
020900051215     c                   eval      tudmult=kmult
021000051219     c                   eval      tudfil=kfgs
021100051215     c                   clear                   tudcli
021200051215     c                   write     tntud000
021300051215     c                   else
021400051215     c                   update    tntud000
021500051215     c                   endif
021600051215     c                   endif
021700051215     c
021800051215     c                   ENDSR
