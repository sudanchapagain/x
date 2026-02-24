Sub Cleaner()
    Dim i As Integer
    Dim j As Integer
    Dim oPres As Presentation
    Set oPres = ActivePresentation
    On Error Resume Next
    With oPres
        For i = .Designs.Count To 1 Step -1
            For j = .Designs(i).SlideMaster.CustomLayouts.Count To 1 Step -1
                .Designs(i).SlideMaster.CustomLayouts(j).Delete
            Next
        Next i
    For i = .Designs.Count To 1 Step -1
        If .Designs(i).SlideMaster.CustomLayouts.Count = 0 Then
            .Designs(i).Delete
        End If
    Next i
    End With
End Sub
