000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300000309     H**  controlla e deleta gli ABI/CAB di PROJ che non sono in Orion
000400890921     H*----------------------------------------------------*
000500000309     F*  param    IMPOSTARE :
000600000309     F*                       1   1 AGG  ----> se =S aggiorna
001200000309     Fanact01L  uF   E           K DISK
001201000309     Fanacr01L  uF   E           K DISK
001300000309     Fcnabi00F  IF   E           K DISK
001302000309     Fqsysprt   o    f  132        PRINTER
009700920821      *
009701000309     C     *LOVAL        SETLL     ANACT01L
009702000310     c                   except    testa
009703000309     C                   DO        *HIVAL
009704000309     C                   READ      ANACT01L                               10
009705000309     C   10              LEAVE
009706000309      *
009707000309     C                   MOVE      ACTABI        KABI
009900000309     C     KABI          SETLL     CNABI00F                               20
009901000309      * Se non trovo l'abi devo deletare ANACT + tutti gli ANACR
009902000309     C                   IF        *IN20 = *OFF
009903000310     c                   except    detabi
009904000310      * deleto il record di ANACT se parametro per aggiornamento = S
009905000310     c                   if        agg = 'S'
009906000310     c                   delete    anact000
009907000310     c                   endif
009908000310      * segnalo e deleto tutti i record di ANACR con lo stesso ABI
009909000310     c                   eval      kabir = actabi
009910000309     c                   exsr      delanacr
009912000309     c                   else
009913000310      * Se trovo l'ABI controllo  tutti i  CAB
009914000310     c                   eval      kabir = actabi
009915000309     c                   exsr      ctranacr
009916000309     c
009917000309     C                   ENDIF
009918000309     C                   ENDDO
009919000309      *
009920000309     C                   SETON                                        LR
010000920821      *
010001000309     C*----------------------------------------------------*
010002000309     C     ctranacr      BEGSR
010003000309     C*----------------------------------------------------*
010004000309     c     kabir         setll     anacr01l
010005000309      *
010006000309     c                   do        *hival
010007000309     c     kabir         reade     anacr01l                               30
010008000309     c   30              leave
010009000309     c                   move      acrabi        kabi
010010000309     c                   move      acrcab        kcab
010011000310     c     keyabi        chain     cnabi00f                           31
010012000309     c                   if        *in31 = *on
010013000310      * se abi/cab non esiste negli archivi di Orion lo segnalo
010014000309     c                   except    detcab
010015000310      * e deleto il record di PJ se parametro per aggiornamento = S
010016000310     c                   if        agg = 'S'
010017000310     c                   delete    anacr000
010018000310     c                   endif
010019000310     c                   endif
010020000309     c                   enddo
010021000310      *
010022000309     C                   ENDSR
010023000309      *
010024000309     C*----------------------------------------------------*
010025000309     C     delanacr      BEGSR
010026000309     C*----------------------------------------------------*
010027000310     c     kabir         setll     anacr01l
010028000310      *
010029000310     c                   do        *hival
010030000310     c     kabir         reade     anacr01l                               30
010031000310     c   30              leave
010037000310     c                   except    anncab
010038000310      * e deleto il record di PJ se parametro per aggiornamento = S
010039000310     c                   if        agg = 'S'
010040000310     c                   delete    anacr000
010041000310     c                   endif
010043000310     c                   enddo
010044000310      *
010045000309     C                   ENDSR
010100910830     C*----------------------------------------------------*
010101000309      *
010200000309     C     *INZSR        BEGSR
010300910830     C*                                                    *
010400910830     C     *ENTRY        PLIST                                                  *
010500000309     C                   PARM                    AGG               1            *
010800910830     C*                                                    *
010900000309     C     KEYABI        KLIST
011000000309     C                   KFLD                    KABI
011100000309     C                   KFLD                    KCAB
011101000309     C*-                                                   *
013300000309     C     *LIKE         DEFINE    ABIABI        KABI
013301000309     C     *LIKE         DEFINE    ABICAB        KCAB
013302000310     C     *LIKE         DEFINE    actabi        kabir
013500920821     C*-                                                   *
013600910830     C                   ENDSR
013700000310     Oqsysprt   e            testa          1  1
013800000310     O                                        +  40 'Elenco abi CAb di Proj non'
013900000310     O                                        +   1 'esistenti in ORION'
014000000310     O          e            detabi         1
014100000310     O                       actabi           +   1
014101000310     O                       actdesist        +   1
014200000310     O                                        +   1 'ABI inesistente'
014201000310     O          e            anncab         1
014202000310     O                       acrabi           +   1
014203000310     O                       actdesist        +   1
014204000310     O                                        +   1 'CAB di ABI inestente: '
014205000310     O                       acrcab           +   1
014206000310     O                       acrdesage        +   1
014207000310     O          e            detcab         1
014208000310     O                       acrabi           +   1
014209000310     O                       actdesist        +   1
014210000310     O                       acrcab           +   1
014211000310     O                       acrdesage        +   1
014212000310     O                                        +   1 'ABI/CAB inesistente'
014300000310
