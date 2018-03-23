000100940224     H*--------------------------------------------------------------*
000203060322     FFIFCEDEL  iF   E             DISK    rename(FIFCEDEL:FCEdel)
000204060321     f                                     prefix(X)
000205060322     FFIFCE01L  uF   E           k DISK
000500900131     C*---------------------------------------------------------------*
000520060320     C*
000521060320     C* CHIAVE LETTURA FIFCE01L - COMPLETA
000522060320     C     KEYFCE        KLIST
000523060321     C                   KFLD                    xFCEFGS
000524060321     C                   KFLD                    xFCEPDR
000525060321     C                   KFLD                    xFCETSR
000526060321     C                   KFLD                    xFCENDC
000527060321     C                   KFLD                    xFCEDDC
000528060321     C                   KFLD                    xFCEAAS
000529060321     C                   KFLD                    xFCELNP
000530060321     C                   KFLD                    xFCENRS
000531060321     C                   KFLD                    xFCENSP
000532060321     C                   KFLD                    xFCECCE
001700030205      *
001800030515     c                   do        *hival
001900060322     c                   read      fifcedel
002000030515     c* fine file
002100060322     c                   if        %eoF(fifcedel)
002200030515     c                   leave
002300030515     c                   end
002401060321      *
002402060322     C     KEYFCE        CHAIN     FIFCE01L                           99
002403060322IF  6C     *IN99         IFEQ      *Off
002404060321     C                   Z-ADD     xFCEFGS       FCEFGS
002405060321     C                   Z-ADD     xFCEPDR       FCEPDR
002406060321     C                   movel     xFCETSR       FCETSR
002407060321     C                   Z-ADD     xFCENDC       FCENDC
002408060321     C                   Z-ADD     xFCEDDC       FCEDDC
002409060321     C                   Z-ADD     xFCEAAS       FCEAAS
002410060321     C                   Z-ADD     xFCELNP       FCELNP
002411060321     C                   Z-ADD     xFCENRS       FCENRS
002412060321     C                   Z-ADD     xFCENSP       FCENSP
002413060321     C                   movel     xFCECCE       FCECCE
002414060321     C                   Z-ADD     xFCEICE       FCEICE
002415060321     C                   MOVEL     xFCESAL       FCESAL
002416060321     C                   MOVEL     xFCECBO       FCECBO
002417060322     C                   delete    fifce000
002419060321E   6C                   ENDIF
002421060321     C*
003100030205     c                   enddo
003200000616     C*
003300000000     C                   SETON                                        LR
003400060320     C*------------------------------------------------------------------------*
