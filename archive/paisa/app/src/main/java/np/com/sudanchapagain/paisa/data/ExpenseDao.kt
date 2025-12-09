package np.com.sudanchapagain.paisa.data

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.Query
import kotlinx.coroutines.flow.Flow

@Dao
interface ExpenseDao {
    @Insert
    suspend fun insertExpense(expense: Expense)

    @Query("SELECT * FROM expenses ORDER BY insertionTime DESC")
    fun getAllExpenses(): Flow<List<Expense>>

    @Query("DELETE FROM expenses")
    suspend fun deleteAllExpenses()
}
