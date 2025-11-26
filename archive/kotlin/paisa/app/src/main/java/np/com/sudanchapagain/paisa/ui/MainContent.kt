package np.com.sudanchapagain.paisa.ui

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.MoreVert
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.key.onKeyEvent
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.room.PrimaryKey
import java.text.SimpleDateFormat
import java.util.*
import np.com.sudanchapagain.paisa.data.Expense

@Composable
fun mainContent(
    viewModel: ExpenseViewModel,
    modifier: Modifier = Modifier
) {
    var inputValue by remember { mutableStateOf("") }
    var expenseList by remember { mutableStateOf<List<Expense>>(emptyList()) }

    LaunchedEffect(Unit) {
        viewModel.expenses.collect { expenses ->
            expenseList = expenses
        }
    }

    Box(
        modifier = modifier.fillMaxSize().padding(16.dp), contentAlignment = Alignment.TopCenter
    ) {
        Column(
            horizontalAlignment = Alignment.CenterHorizontally, modifier = Modifier.fillMaxSize()
        ) {
            Row(
                modifier = Modifier.fillMaxWidth().padding(vertical = 16.dp),
                verticalAlignment = Alignment.CenterVertically
            ) {
                Text(
                    text = "paisa", style = TextStyle(
                        color = Color.Black,
                        fontSize = 24.sp,
                        fontWeight = FontWeight.Bold,
                    ), modifier = Modifier.weight(1f)
                )

                IconButton(onClick = { /* TODO */ }) {
                    Icon(
                        imageVector = Icons.Filled.Search, contentDescription = "Search Icon"
                    )
                }

                IconButton(onClick = { /* TODO */ }) {
                    Icon(
                        imageVector = Icons.Filled.MoreVert, contentDescription = "More Options Icon"
                    )
                }
            }

            BasicTextField(
                value = inputValue,
                onValueChange = { inputValue = it },
                modifier = Modifier
                    .fillMaxWidth()
                    .background(
                        Color(0xfff1f1f1),
                        shape = RoundedCornerShape(8.dp)
                    )
                    .padding(12.dp),
                textStyle = TextStyle(color = Color.Black, fontSize = 16.sp),
                decorationBox = { innerTextField ->
                    if (inputValue.isEmpty()) {
                        Text(
                            text = "Enter your expenses here!",
                            style = TextStyle(color = Color.Gray, fontSize = 16.sp)
                        )
                    }
                    innerTextField()
                },
                keyboardOptions = KeyboardOptions(imeAction = ImeAction.Done),
                keyboardActions = KeyboardActions(
                    onDone = {
                        if (inputValue.isNotEmpty()) {
                            val expense = parseExpense(inputValue)
                            if (expense != null) {
                                viewModel.addExpense(expense)
                                inputValue = ""
                            }
                        }
                    })
            )


            Spacer(modifier = Modifier.height(16.dp))
            expenseTable(expenseList)
        }
    }
}

@Composable
fun expenseTable(expenses: List<Expense>) {
    Column(
        modifier = Modifier.fillMaxWidth().padding(16.dp)
    ) {
        LazyColumn {
            items(expenses) { expense ->
                Row(modifier = Modifier.fillMaxWidth().padding(8.dp)) {
                    Text(text = expense.date, modifier = Modifier.weight(1f))
                    Text(text = expense.description, modifier = Modifier.weight(2f))
                    Text(text = expense.category, modifier = Modifier.weight(1f))
                    Text(text = "$${expense.amount}", modifier = Modifier.weight(1f))
                }
            }
        }
    }
}

fun parseExpense(input: String): Expense? {
    // TODO: yet to test, chatgpt wrote this regex
    val regex = """@(\d{1,2})\s*(\D+)\s*\$(\d+)\s*#(\w+)""".toRegex()
    val matchResult = regex.find(input)

    return if (matchResult != null) {
        val (date, description, amount, category) = matchResult.destructured
        val parsedDate = parseDate(date)
        val parsedAmount = amount.toDouble()

        Expense(
            date = parsedDate,
            description = description,
            category = category,
            amount = parsedAmount,
            id = 0,
            insertionTime = System.currentTimeMillis()
        )
    } else {
        null
    }
}

fun parseDate(date: String): String {
    return "$date Jan"
}
