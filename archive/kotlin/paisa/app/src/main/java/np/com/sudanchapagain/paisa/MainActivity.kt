package np.com.sudanchapagain.paisa

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.lifecycle.ViewModelProvider
import np.com.sudanchapagain.paisa.data.ExpenseDatabase
import np.com.sudanchapagain.paisa.data.ExpenseRepository
import np.com.sudanchapagain.paisa.ui.ExpenseViewModel
import np.com.sudanchapagain.paisa.ui.ExpenseViewModelFactory
import np.com.sudanchapagain.paisa.ui.theme.*
import np.com.sudanchapagain.paisa.ui.mainContent

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()

        val database = ExpenseDatabase.getDatabase(applicationContext)
        val repository = ExpenseRepository(database.expenseDao())
        val viewModelFactory = ExpenseViewModelFactory(repository)
        val viewModel = ViewModelProvider(this, viewModelFactory)[ExpenseViewModel::class.java]

        setContent {
            paisaTheme {
                mainContent(viewModel = viewModel)
            }
        }
    }
}
